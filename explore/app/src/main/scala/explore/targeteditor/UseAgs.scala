// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
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
 * What AGS produced for an observation: the catalog candidates around the base, the analysis of
 * those candidates at the angles to test and, if the guide star was picked by hand, its state.
 */
case class AgsData(
  candidates:      Pot[Option[List[GuideStarCandidate]]],
  results:         AgsCalculationResults,
  manualGuideStar: Option[ManualGuideStar] = none
)

object AgsData:
  val Empty: AgsData =
    AgsData(Pot.pending, AgsCalculationResults.Empty)

  private given Reusability[ManualGuideStar] = Reusability.byEq

  given Reusability[AgsData] =
    // We don't want to check every candidate, the length is enough to know if the catalog changed.
    Reusability.by(d => (d.candidates.map(_.map(_.length)), d.results, d.manualGuideStar))

object UseAgs:
  // Position-angle changes debouncing time
  private val AgsDebounceDelay: FiniteDuration = 500.millis

  // only compare candidates by id
  private given Reusability[GuideStarCandidate] = Reusability.by(_.id)

  private def gaiaSourceId(name: NonEmptyString): Option[Long] =
    GuideStarName.from(name.value).toOption.flatMap(_.toGaiaSourceId)

  /**
   * Runs guide star selection for an observation.
   */
  def useAgs(
    obsTargets:         Option[ObservationTargets],
    obsTime:            Option[Instant],
    positions:          ObsPositions,
    obsConf:            ObsConfiguration,
    guideStarSelection: View[GuideStarSelection]
  )(ctx: AppContext[IO]): HookResult[AgsData] =
    for
      candidates       <-
        useEffectResultWithDeps(
          (obsTime.map(SiderealDiscretizedObsTime(_, obsConf.posAngleConstraint)),
           positions.baseTracking,
           obsConf.explicitBase,
           obsConf.obsModeType,
           obsConf.guideProbe,
           obsConf.needGuideStar
          )
        ): (discretizedObsTime, oTracking, explicitBase, obsModeType, guideProbe, needsAGS) =>
          import ctx.given

          // Prefer the explicit base override as the catalog search center
          val searchTracking: Option[Tracking] =
            explicitBase.map(Tracking.constant).orElse(oTracking)

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
      // A star picked by hand may lie outside the catalog search (area, brightness or row limit).
      // It is then looked up by id and added to the candidates, so AGS ranks it with the rest.
      catalogCands      = candidates.value.toOption.flatten
      extraStarName     = catalogCands.flatMap: cands =>
                            obsConf.remoteGSName.filterNot(n => cands.exists(_.name === n))
      extraStar        <- useEffectResultWithDeps(extraStarName): name =>
                            import ctx.given

                            name
                              .flatMap(gaiaSourceId)
                              .fold(none[GuideStarCandidate].pure[IO]): id =>
                                (obsConf.agsState.foldMap(_.async.set(AgsState.LoadingCandidates)) *>
                                  CatalogClient[IO]
                                    .requestSingle(CatalogMessage.GSByIdRequest(id))
                                    .map(_.flatten))
                                  .guarantee(obsConf.agsState.foldMap(_.async.set(AgsState.Idle)))
                              .tupleLeft(name)
      // Only wait for the lookup when there is one, keyed on the name it was made for.
      allCands          = catalogCands.flatMap: cands =>
                            extraStarName.fold(cands.some): name =>
                              extraStar.value.toOption
                                .collect { case (n, star) if n.contains(name) => star }
                                .map(star => cands ++ star.toList)
      agsCalcProps     <- useMemo(
                            (obsTargets.map(_.focus.id),
                             obsTime,
                             obsConf.constraints,
                             obsConf.agsWavelength,
                             obsConf.configuration,
                             obsConf.obsModeType,
                             obsConf.guidedAcqOffsets,
                             obsConf.guidedSciOffsets,
                             allCands,
                             obsConf.guideProbe,
                             obsConf.altairMode
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
                                  guideProbe,
                                  altairMode
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
                                guideProbe,
                                altairMode
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
                                .mod(_.resetKeepingName)
                                .when_(changed && obsConf.needGuideStar)
      // Debounced twin of `anglesToTest` for AGS consumption. We push the live value in on every
      // (structural) change, the debounced output lags by `AgsDebounceDelay`
      anglesDebounce   <- useDebounce(anglesToTest, AgsDebounceDelay.toMillis.toInt)
      _                <- useEffectWithDeps(anglesToTest): v =>
                            anglesDebounce.set(v)
      // request AGS calculation (on the debounced angles, see `anglesDebounce`)
      results          <- useAgsCalculation(
                            positions.coords.toOption.flatMap(_.toOption),
                            agsCalcProps.value,
                            anglesDebounce.debouncedValue,
                            obsConf.posAngleConstraint.isDefined,
                            obsConf.agsState,
                            guideStarSelection,
                            obsConf.needGuideStar
                          )(ctx)
      // In case the selected name changes remotely
      _                <- useEffectWithDeps((obsConf.remoteGSName, results.constrained)): (n, resultsPot) =>
                            resultsPot.toOption.foldMap: results =>
                              guideStarSelection.set(results.select(n))
      // Reported only once AGS has settled, so a pick is not flagged while it is being checked.
      manualGuideStar   =
        for
          name   <- obsConf.remoteGSName
          usable <- results.constrained.toOption
          if obsConf.agsState.forall(_.get.isIdle)
        yield
          val notFound: Boolean =
            extraStarName.contains(name) &&
              (gaiaSourceId(name).isEmpty ||
                extraStar.value.toOption.exists { case (n, star) =>
                  n.contains(name) && star.isEmpty
                })
          if usable.exists(_.target.name === name) then ManualGuideStar.Selected
          else if notFound then ManualGuideStar.NotFound
          else ManualGuideStar.Unusable
    yield AgsData(candidates.value.value, results, manualGuideStar)
