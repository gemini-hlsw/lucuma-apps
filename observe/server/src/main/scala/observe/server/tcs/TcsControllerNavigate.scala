// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.tcs

import cats.data.NonEmptySet
import cats.effect.Async
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import clue.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.Site
import lucuma.core.util.TimeSpan
import lucuma.schemas.NavigateDB
import lucuma.schemas.NavigateDB.Types.ConfigureStepInput
import lucuma.schemas.NavigateDB.Types.DistanceInput
import lucuma.schemas.NavigateDB.Types.LightPathInput
import lucuma.schemas.ObservationDB.Types.OffsetInput
import lucuma.schemas.model.navigate.LightSinkVariant
import lucuma.schemas.model.navigate.OperationResult
import lucuma.schemas.odb.input.*
import observe.common.NavigateQueriesGQL.ConfigureStepMutation
import observe.server.Length
import observe.server.ObserveFailure
import observe.server.tcs.TcsController.*
import org.typelevel.log4cats.Logger

import java.time.temporal.ChronoUnit

/**
 * Configures the telescope for a step through Navigate's `configureStep` mutation, instead of
 * commanding the TCS directly.
 */
trait TcsControllerNavigate[F[_], S <: Site] {
  def applyBasicConfig(
    subsystems: NonEmptySet[Subsystem],
    tcs:        BasicTcsConfig[S]
  ): F[Unit]

  def notifyObserveStart: F[Unit]

  def notifyObserveEnd: F[Unit]

  def nod(
    subsystems: NonEmptySet[Subsystem],
    offset:     InstrumentOffset,
    guided:     Boolean,
    tcs:        BasicTcsConfig[S]
  ): F[Unit]
}

object TcsControllerNavigate {

  val DefaultTimeout: TimeSpan = TimeSpan.unsafeFromDuration(10, ChronoUnit.SECONDS)

  private final class TcsControllerNavigateImpl[F[_]: {Async, Logger}, S <: Site](
    epicsSys: TcsEpics[F]
  )(using FetchClient[F, NavigateDB])
      extends TcsControllerNavigate[F, S] {
    private val L: Logger[F] = Logger[F]

    private def configureStep(input: ConfigureStepInput): F[Unit] =
      for {
        _       <- L.debug(s"Send configureStep to Navigate: $input")
        outcome <- ConfigureStepMutation[F].execute(input).raiseGraphQLErrors
        _       <-
          ObserveFailure
            .Execution(
              s"Navigate failed to configure the step${outcome.configureStep.msg
                  .foldMap(m => s": $m")}"
            )
            .raiseError[F, Unit]
            .whenA(outcome.configureStep.result === OperationResult.Failure)
        _       <- L.debug("Navigate completed configureStep")
      } yield ()

    override def applyBasicConfig(
      subsystems: NonEmptySet[Subsystem],
      tcs:        BasicTcsConfig[S]
    ): F[Unit] =
      L.debug(s"TCS configuration for subsystems $subsystems: ${tcs.show}") *>
        configureStep(configureStepInput(subsystems, tcs))

    // Navigate has no equivalent for these commands yet, so they are still sent to the TCS.
    override def notifyObserveStart: F[Unit] =
      L.debug("Send observe to TCS") *>
        epicsSys.observe.mark *>
        epicsSys.post(DefaultTimeout) *>
        L.debug("Observe command sent to TCS")

    override def notifyObserveEnd: F[Unit] =
      L.debug("Send endObserve to TCS") *>
        epicsSys.endObserve.mark *>
        epicsSys.post(DefaultTimeout) *>
        L.debug("endObserve command sent to TCS")

    override def nod(
      subsystems: NonEmptySet[Subsystem],
      offset:     InstrumentOffset,
      guided:     Boolean,
      tcs:        BasicTcsConfig[S]
    ): F[Unit] =
      L.debug(s"Nod to offset $offset, guided = $guided") *>
        configureStep(nodInput(subsystems, offset, guided, tcs))
  }

  def apply[F[_]: {Async, Logger}, S <: Site](epicsSys: TcsEpics[F])(using
    FetchClient[F, NavigateDB]
  ): TcsControllerNavigate[F, S] =
    new TcsControllerNavigateImpl[F, S](epicsSys)

  /**
   * Builds the `configureStep` input for a step. As when commanding the TCS directly, the offset
   * and wavelength are only set if the mount is part of the configured subsystems, the light path
   * only if the A&G unit is, and the instrument defocus only if M2 is.
   */
  def configureStepInput[S <: Site](
    subsystems: NonEmptySet[Subsystem],
    tcs:        BasicTcsConfig[S]
  ): ConfigureStepInput =
    ConfigureStepInput(
      offset = tcs.tc.offsetA
        .filter(_ => subsystems.contains(Subsystem.Mount))
        .map(offsetInput)
        .orIgnore,
      wavelength = tcs.tc.wavelA
        .filter(_ => subsystems.contains(Subsystem.Mount))
        .map(_.toInput)
        .orIgnore,
      lightPath = subsystems
        .contains(Subsystem.AGUnit)
        .guard[Option]
        .as(lightPathInput(tcs.agc.sfPos, tcs.inst.instrument))
        .orIgnore,
      defocus = tcs.tc.defocusB
        .filter(_ => subsystems.contains(Subsystem.M2))
        .map(distanceInput)
        .orIgnore,
      guiding = isGuiding(subsystems, tcs.gds)
    )

  /**
   * Builds the `configureStep` input for a nod. A nod only moves the telescope to the new offset,
   * leaving guiding off if the nod position is not guided.
   */
  def nodInput[S <: Site](
    subsystems: NonEmptySet[Subsystem],
    offset:     InstrumentOffset,
    guided:     Boolean,
    tcs:        BasicTcsConfig[S]
  ): ConfigureStepInput =
    ConfigureStepInput(
      offset = subsystems.contains(Subsystem.Mount).guard[Option].as(offsetInput(offset)).orIgnore,
      guiding = guided && isGuiding(subsystems, tcs.gds)
    )

  /**
   * The step is guided if any of the guiders being configured is active. Which guiders are active
   * already combines the step's guide state with the guide configuration set by the operator.
   */
  def isGuiding(subsystems: NonEmptySet[Subsystem], gds: GuidersConfig): Boolean =
    List(
      Subsystem.PWFS1 -> gds.pwfs1.value,
      Subsystem.PWFS2 -> gds.pwfs2.value,
      Subsystem.OIWFS -> gds.oiwfs.value
    ).exists { case (s, g) => subsystems.contains(s) && g.isActive }

  def offsetInput(o: InstrumentOffset): OffsetInput = o.toOffset.toInput

  def distanceInput(l: Length): DistanceInput =
    DistanceInput.Micrometers(l.toMicrometers.value)

  def lightPathInput(lp: LightPath, instrument: Instrument): LightPathInput =
    LightPathInput(
      from = lp.source,
      instrument = instrument,
      lightSinkVariant = lightSinkVariant(lp.sink).orIgnore
    )

  private def lightSinkVariant(sink: LightSinkName): Option[LightSinkVariant] =
    sink match {
      case LightSinkName.Gmos_Ifu => LightSinkVariant.GmosIfu.some
      case LightSinkName.Niri_f6  => LightSinkVariant.NiriF6.some
      case LightSinkName.Niri_f14 => LightSinkVariant.NiriF14.some
      case LightSinkName.Niri_f32 => LightSinkVariant.NiriF32.some
      case _                      => none
    }
}
