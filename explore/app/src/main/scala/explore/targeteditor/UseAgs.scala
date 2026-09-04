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
import crystal.react.syntax.pot.given
import explore.events.*
import explore.model.*
import explore.model.GuideStarSelection.*
import explore.model.WorkerClients.*
import explore.model.boopickle.*
import explore.model.boopickle.CatalogPicklers.given
import explore.model.enums.AgsState
import explore.model.reusability.given
import explore.targeteditor.UseAgsCalculation.*
import japgolly.scalajs.react.*
import lucuma.ags.*
import lucuma.core.math.Angle
import lucuma.core.model.Tracking
import lucuma.react.primereact.hooks.useDebounce
import lucuma.schemas.model.syntax.minimizeEphemeris
import lucuma.ui.reusability.given

import java.time.Instant
import scala.concurrent.duration.*

/**
 * What AGS produced for an observation: the catalog candidates around the base and the analysis of
 * those candidates at the angles to test.
 */
case class AgsData(
  candidates: Pot[Option[List[GuideStarCandidate]]],
  results:    AgsCalculationResults
)

object AgsData:
  val Empty: AgsData =
    AgsData(Pot.pending, AgsCalculationResults(Pot.pending, Pot.pending))

  given Reusability[AgsData] =
    Reusability.by(d => (d.candidates.toOption.flatten.map(_.length), d.results))

object UseAgs:
  // Position-angle changes debouncing time
  private val AgsDebounceDelay: FiniteDuration = 500.millis

  // only compare candidates by id
  private given Reusability[GuideStarCandidate] = Reusability.by(_.id)

  /**
   * Runs guide star selection for an observation. It lives with the observation, not with Aladin,
   * so the selected guide star follows time and configuration changes even while the target tile
   * is minimized and Aladin is unmounted. Aladin only draws what this produces.
   */
  def useAgs(
    obsTargets:         Option[ObservationTargets],
    obsTime:            Option[Instant],
    trackingMap:        Pot[ErrorMsgOr[RegionOrTrackingMap]],
    obsConf:            ObsConfiguration,
    guideStarSelection: View[GuideStarSelection]
  )(ctx: AppContext[IO]): HookResult[AgsData] =
    for
      obsCoords        <- useMemo(
                            (obsTargets, obsTime, trackingMap, obsConf.targetViz, obsConf.explicitBase)
                          ): (targets, at, trPot, targetViz, explicitBase) =>
                            (targets, at, trPot.toOption).tupled.flatMap: (ts, at, tr) =>
                              if (ts.hasUnresolvedTargetOfOpportunity)
                                ObservationTargetsCoordinatesAt.emptyAt(at).toOption
                              else
                                tr.toOption.flatMap: map =>
                                  ObservationTargetsCoordinatesAt(
                                    at,
                                    ts,
                                    map,
                                    targetViz.slots,
                                    explicitBase
                                  ).toOption
      oBaseTracking    <- useMemo((obsTargets, trackingMap.toOption.flatMap(_.toOption))):
                            (targets, trackings) =>
                              (targets, trackings).flatMapN(_.optAsterismTracking(_))
      candidates       <-
        useEffectResultWithDeps(
          (obsTime.map(SiderealDiscretizedObsTime(_, obsConf.posAngleConstraint)),
           oBaseTracking,
           obsConf.explicitBase,
           obsConf.obsModeType,
           obsConf.guideProbe,
           obsConf.needGuideStar
          )
        ): (discretizedObsTime, oTracking, explicitBase, obsModeType, guideProbe, needsAGS) =>
          import ctx.given

          // Prefer the explicit base override as the catalog search center
          val searchTracking: Option[Tracking] =
            explicitBase.map(Tracking.constant).orElse(oTracking.value)

          (discretizedObsTime, obsModeType, searchTracking)
            .mapN: (discretizedObsTime, _, baseTracking) =>
              if (needsAGS)
                (for
                  _          <- obsConf.agsState.foldMap(_.async.set(AgsState.LoadingCandidates))
                  candidates <-
                    guideProbe.foldMap: gp =>
                      CatalogClient[IO]
                        .requestSingle:
                          CatalogMessage.GSRequest(
                            baseTracking.minimizeEphemeris(discretizedObsTime.obsTime),
                            discretizedObsTime.obsTime,
                            gp
                          )
                yield candidates)
                  .guarantee(obsConf.agsState.foldMap(_.async.set(AgsState.Idle)))
              else none.pure[IO]
            .getOrElse(List.empty.some.pure[IO])
      agsCalcProps     <- useMemo(
                            (obsTargets.map(_.focus.id),
                             obsTime,
                             obsConf.constraints,
                             obsConf.agsWavelength,
                             obsConf.configuration,
                             obsConf.obsModeType,
                             obsConf.guidedAcqOffsets,
                             obsConf.guidedSciOffsets,
                             candidates.value.toOption.flatten,
                             obsConf.trackType
                            )
                          ):
                            case (Some(focusedId),
                                  Some(obsTime),
                                  Some(constraints),
                                  Some(agsWavelength),
                                  observingMode,
                                  Some(obsModeType),
                                  acqOffsets,
                                  sciOffsets,
                                  Some(cands),
                                  trackType
                                ) =>
                              AgsCalcProps(
                                focusedId,
                                obsTime,
                                constraints,
                                agsWavelength,
                                observingMode,
                                obsModeType,
                                acqOffsets,
                                sciOffsets,
                                cands,
                                trackType
                              ).some
                            case _ => none
      anglesToTest      = obsConf.anglesToTest
      // The guide star is picked for a set of PAs. When they change (e.g. a new observation time
      // moves the average PA) the selection must be redone. The first computed value is skipped
      // so the selection loaded from the ODB is kept.
      prevAnglesToTest <- useRef(none[NonEmptyList[Angle]])
      _                <- useEffectWithDeps(anglesToTest): angles =>
                            val changed =
                              (prevAnglesToTest.value, angles).mapN(_ =!= _).exists(identity)
                            prevAnglesToTest.set(angles.orElse(prevAnglesToTest.value)) >>
                              guideStarSelection
                                .set(GuideStarSelection.Default)
                                .when_(changed && obsConf.needGuideStar)
      // Debounced twin of `anglesToTest` for AGS consumption. We push the live value in on every
      // (structural) change, the debounced output lags by `AgsDebounceDelay`
      anglesDebounce   <- useDebounce(anglesToTest, AgsDebounceDelay.toMillis.toInt)
      _                <- useEffectWithDeps(anglesToTest): v =>
                            anglesDebounce.set(v)
      // request AGS calculation (on the debounced angles, see `anglesDebounce`)
      results          <- useAgsCalculation(
                            obsCoords.value,
                            agsCalcProps.value,
                            anglesDebounce.debouncedValue,
                            obsConf.posAngleConstraint.isDefined,
                            obsConf.agsState,
                            guideStarSelection,
                            obsConf.needGuideStar
                          )(ctx)
      // In case the selected name changes remotely
      _                <- useEffectWithDeps((obsConf.remoteGSName, results.constrained)):
                            (n, resultsPot) =>
                              resultsPot.toOption.foldMap: results =>
                                val newGss =
                                  n.fold(AgsSelection(results.headOption.tupleLeft(0))):
                                    results.pick
                                guideStarSelection.set(newGss)
    yield AgsData(candidates.value.value, results)
