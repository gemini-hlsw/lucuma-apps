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
import explore.model.WorkerClients.*
import explore.model.boopickle.CatalogPicklers.given
import explore.model.enums.AgsState
import explore.model.reusability.given
import japgolly.scalajs.react.*
import lucuma.ags.*
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Target
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import lucuma.ui.reusability.given

import java.time.Instant

import explore.model.GuideStarSelection.*

case class AgsCalcProps(
  focusedId:         Target.Id,
  obsTime:           Instant,
  constraints:       ConstraintSet,
  centralWavelength: CentralWavelength,
  observingMode:     Option[BasicConfiguration],
  obsModeType:       ObservingModeType,
  acqOffsets:        Option[AcquisitionOffsets],
  sciOffsets:        Option[ScienceOffsets],
  candidates:        List[GuideStarCandidate]
)

object AgsCalcProps:
  given Reusability[AgsCalcProps] = Reusability.by: p =>
    (p.focusedId,
     p.obsTime,
     p.constraints,
     p.centralWavelength,
     p.observingMode,
     p.obsModeType,
     p.acqOffsets,
     p.sciOffsets,
     p.candidates.length
    )

case class AgsCalculationResults(
  constrained:   Pot[List[AgsAnalysis.Usable]],
  unconstrained: Pot[List[AgsAnalysis.Usable]]
)

object AgsCalculationResults:
  given Reusability[AgsCalculationResults] = Reusability.by: r =>
    (r.constrained.toOption, r.unconstrained.toOption)

object UseAgsCalculation:

  private def agsParams(
    obsModeType:   ObservingModeType,
    observingMode: Option[BasicConfiguration]
  ): Option[AgsParams] =
    obsModeType match
      case ObservingModeType.Flamingos2LongSlit =>
        observingMode
          .collect:
            case BasicConfiguration.Flamingos2LongSlit(fpu = fpu) => fpu
          .map: f =>
            AgsParams.Flamingos2AgsParams(
              Flamingos2LyotWheel.F16,
              Flamingos2FpuMask.Builtin(f),
              PortDisposition.Side
            )

      case ObservingModeType.GmosNorthLongSlit | ObservingModeType.GmosSouthLongSlit =>
        val fpu: Option[Either[GmosNorthFpu, GmosSouthFpu]] =
          observingMode.flatMap(_.gmosFpuAlternative)
        AgsParams.GmosAgsParams(fpu, PortDisposition.Side).some

      case ObservingModeType.GmosNorthImaging | ObservingModeType.GmosSouthImaging =>
        throw new NotImplementedError("Gmos Imaging not implemented")

  private def runAgsQuery(
    props:          AgsCalcProps,
    obsCoords:      ObservationTargetsCoordinatesAt,
    angles:         NonEmptyList[Angle],
    processResults: Option[List[AgsAnalysis.Usable]] => IO[Unit]
  )(ctx: AppContext[IO]): IO[Unit] =
    import ctx.given

    obsCoords.baseCoords.map { baseCoords =>
      val params = agsParams(props.obsModeType, props.observingMode)

      params
        .map: p =>
          AgsMessage.AgsRequest(
            props.focusedId,
            props.obsTime,
            props.constraints,
            props.centralWavelength.value,
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
          AgsClient[IO]
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
      // Constrained AGS calculation
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
                                      runAgsQuery(props, obsCoords, angles, processResults)(
                                        ctx
                                      )
                                    process.guarantee(state.async.set(AgsState.Idle))

                                  guideStarSelection.set(AgsSelection(none)).toAsync *>
                                    query.orEmpty.unlessA(guideStarSelection.get.isOverride)

                                case _ => IO.unit
      // Unconstrained AGS calculation
      _                    <- useEffectWithDeps(
                                (obsCoords, props, hasConstraint, needsAGS)
                              ):
                                case (Some(obsCoords), Some(props), true, true) if props.candidates.nonEmpty =>
                                  UnconstrainedAngles
                                    .map: angles =>
                                      runAgsQuery(
                                        props,
                                        obsCoords,
                                        angles,
                                        r => r.map(l => unconstrainedResults.setState(Pot.Ready(l))).getOrEmpty.toAsync
                                      )(ctx)
                                    .orEmpty

                                case _ => IO.unit
    } yield AgsCalculationResults(constrainedResults.value, unconstrainedResults.value)
