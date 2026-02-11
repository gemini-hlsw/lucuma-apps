// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import boopickle.DefaultBasic.*
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import explore.events.*
import explore.model.*
import explore.model.GuideStarSelection.*
import explore.model.boopickle.CatalogPicklers.given
import explore.model.enums.AgsState
import explore.model.reusability.given
import japgolly.scalajs.react.*
import lucuma.ags.*
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.TrackType
import lucuma.core.math.Angle
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Target
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.schemas.model.AGSWavelength
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.reusability.given
import workers.WorkerClient

import java.time.Instant

case class AgsCalcProps(
  focusedId:     Target.Id,
  obsTime:       Instant,
  constraints:   ConstraintSet,
  agsWavelength: AGSWavelength,
  observingMode: Option[BasicConfiguration],
  obsModeType:   ObservingModeType,
  acqOffsets:    Option[AcquisitionOffsets],
  sciOffsets:    Option[ScienceOffsets],
  candidates:    List[GuideStarCandidate],
  trackType:     Option[TrackType]
):
  lazy val guideProbe: Option[GuideProbe] =
    observingMode.map(_.guideProbe(trackType))

object AgsCalcProps:
  given Reusability[AgsCalcProps] = Reusability.by: p =>
    (p.focusedId,
     p.obsTime,
     p.constraints,
     p.agsWavelength,
     p.observingMode,
     p.obsModeType,
     p.acqOffsets,
     p.sciOffsets,
     p.candidates.length,
     p.trackType
    )

case class AgsCalculationResults(
  constrained:   Pot[List[AgsAnalysis.Usable]],
  unconstrained: Pot[List[AgsAnalysis.Usable]]
)

object AgsCalculationResults:
  given Reusability[AgsCalculationResults] = Reusability.by: r =>
    // Should we reuse just by length too?
    (r.constrained.toOption, r.unconstrained.toOption)

object UseAgsCalculation:

  private def applyGuideProbe(
    base:       Option[AgsParams],
    guideProbe: Option[GuideProbe]
  ): Option[AgsParams] =
    guideProbe match
      case Some(GuideProbe.PWFS1) =>
        base.map:
          case p: AgsParams.GmosLongSlit       => p.withPWFS1
          case p: AgsParams.GmosImaging        => p.withPWFS1
          case p: AgsParams.Flamingos2LongSlit => p.withPWFS1
          case p: AgsParams.Igrins2LongSlit    => p.withPWFS1
      case Some(GuideProbe.PWFS2) =>
        base.map:
          case p: AgsParams.GmosLongSlit       => p.withPWFS2
          case p: AgsParams.GmosImaging        => p.withPWFS2
          case p: AgsParams.Flamingos2LongSlit => p.withPWFS2
          case p: AgsParams.Igrins2LongSlit    => p.withPWFS2
      case _                      => base

  private def agsParams(
    obsModeType:   ObservingModeType,
    observingMode: Option[BasicConfiguration],
    guideProbe:    Option[GuideProbe],
    port:          PortDisposition = PortDisposition.Side
  ): Option[AgsParams] =
    obsModeType match
      case ObservingModeType.Flamingos2LongSlit =>
        val base = observingMode
          .flatMap(_.f2Fpu)
          .map: fpu =>
            AgsParams.Flamingos2LongSlit(
              Flamingos2LyotWheel.F16,
              Flamingos2FpuMask.Builtin(fpu),
              port
            )
        applyGuideProbe(base, guideProbe)

      case ObservingModeType.GmosNorthLongSlit | ObservingModeType.GmosSouthLongSlit =>
        val base = observingMode
          .flatMap(_.gmosFpuAlternative)
          .map(AgsParams.GmosLongSlit(_, port))
        applyGuideProbe(base, guideProbe)

      case ObservingModeType.GmosNorthImaging | ObservingModeType.GmosSouthImaging =>
        val base = Some(AgsParams.GmosImaging(port))
        applyGuideProbe(base, guideProbe)

      case ObservingModeType.Igrins2LongSlit =>
        none

  private def runAgsQuery(
    props:          AgsCalcProps,
    obsCoords:      ObservationTargetsCoordinatesAt,
    angles:         NonEmptyList[Angle],
    processResults: Option[List[AgsAnalysis.Usable]] => IO[Unit],
    agsClient:      WorkerClient[IO, AgsMessage.Request]
  )(ctx: AppContext[IO]): IO[Unit] =
    obsCoords.baseCoords.map { baseCoords =>
      val params = agsParams(props.obsModeType, props.observingMode, props.guideProbe)

      params
        .map: p =>
          AgsMessage.AgsRequest(
            props.focusedId,
            props.obsTime,
            props.constraints,
            props.agsWavelength,
            baseCoords,
            obsCoords.scienceCoords,
            obsCoords.blindOffsetCoords,
            angles,
            props.acqOffsets,
            props.sciOffsets,
            p,
            props.candidates
          )
        .map: req =>
          agsClient
            .requestSingle(req)
            .flatMap(processResults)
            .handleErrorWith(t =>
              ctx.logger.error(t)(s"Error on ags calculation ${t.getMessage()}")
            )
        .getOrElse(IO.unit)
    }.orEmpty

  def useAgsCalculation(
    obsCoords:          Option[ObservationTargetsCoordinatesAt],
    props:              Option[AgsCalcProps],
    anglesToTest:       Option[NonEmptyList[Angle]],
    hasConstraint:      Boolean,
    agsState:           Option[View[AgsState]],
    guideStarSelection: View[GuideStarSelection],
    needsAGS:           Boolean
  )(ctx: AppContext[IO]): HookResult[AgsCalculationResults] =
    import ctx.given

    for {
      constrainedResults   <- useSerialState(Pot.pending[List[AgsAnalysis.Usable]])
      unconstrainedResults <- useSerialState(Pot.pending[List[AgsAnalysis.Usable]])
      // AGS calculation for the obs pa constraint
      _                    <- useEffectWithDeps(
                                (obsCoords, props, anglesToTest, needsAGS)
                              ):
                                case (Some(obsCoords), Some(props), Some(angles), true) if props.candidates.nonEmpty =>
                                  def processResults(r: Option[List[AgsAnalysis.Usable]]): IO[Unit] =
                                    (for
                                      _ <- r.map(l => constrainedResults.setState(Pot.Ready(l))).getOrEmpty
                                      _ <-
                                        val index      = 0.some.filter(_ => r.exists(_.nonEmpty))
                                        val selectedGS = index.flatMap(i => r.flatMap(_.lift(i)))
                                        guideStarSelection
                                          .mod:
                                            case AgsSelection(_)               =>
                                              AgsSelection(selectedGS.tupleLeft(0))
                                            case rem @ RemoteGSSelection(name) =>
                                              r.map(_.pick(name)).getOrElse(rem)
                                            case a: AgsOverride                =>
                                              a
                                    yield ()).toAsync

                                  val query = agsState.map: state =>
                                    val process = state.set(AgsState.Calculating).toAsync *>
                                      runAgsQuery(props, obsCoords, angles, processResults, ctx.workerClients.ags)(
                                        ctx
                                      )
                                    process.guarantee(state.async.set(AgsState.Idle))

                                  guideStarSelection.set(AgsSelection(none)).toAsync *>
                                    query.orEmpty.unlessA(guideStarSelection.get.isOverride)

                                case _ => IO.unit
      // AGS for uconstrained angles
      _                    <- useEffectWithDeps(
                                (obsCoords, props, hasConstraint, needsAGS)
                              ):
                                case (Some(obsCoords), Some(props), true, true) if props.candidates.nonEmpty =>
                                  runAgsQuery(
                                    props,
                                    obsCoords,
                                    UnconstrainedAngles,
                                    r => r.map(l => unconstrainedResults.setState(Pot.Ready(l))).getOrEmpty.toAsync,
                                    ctx.workerClients.agsUnconstrained
                                  )(ctx)

                                case _ => IO.unit
    } yield AgsCalculationResults(constrainedResults.value, unconstrainedResults.value)
