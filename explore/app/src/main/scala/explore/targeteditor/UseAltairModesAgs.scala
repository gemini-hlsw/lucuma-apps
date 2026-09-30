// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import boopickle.DefaultBasic.*
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.hooks.*
import explore.events.*
import explore.model.*
import explore.model.WorkerClients.*
import explore.model.boopickle.*
import explore.model.boopickle.CatalogPicklers.given
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.modes.AltairModeRows
import japgolly.scalajs.react.*
import lucuma.ags.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.itc.AltairParameters
import lucuma.odb.data.AltairConfiguration
import lucuma.react.primereact.hooks.useDebounce
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.syntax.minimizeEphemeris
import lucuma.ui.reusability.given
import lucuma.ui.visualization.agsParams

import java.time.Instant
import scala.concurrent.duration.*

object UseAltairModesAgs:
  private val AgsDebounceDelay: FiniteDuration = 500.millis

  private given Reusability[GuideStarCandidate] = Reusability.by(_.id)

  // The Altair WFS patrol field does not depend on the GNIRS slit, prism, grating or camera; only
  // the wavelength matters, through the R limits. So any GNIRS long slit setup will do.
  private def representativeGnirs(wavelength: Wavelength): BasicConfiguration =
    BasicConfiguration.GnirsSpectroscopy(
      GnirsFilter.Order4,
      GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_30),
      GnirsPrism.Mirror,
      GnirsGrating.D32,
      GnirsCamera.ShortBlue,
      CentralWavelength(wavelength)
    )

  // The requirements may not have a wavelength yet; the H band is the typical Altair use.
  private val FallbackWavelength: Wavelength = GnirsFilter.Order4.centralWavelength

  private def altairParameters(
    targetId:    Target.Id,
    obsTime:     Instant,
    constraints: ConstraintSet,
    baseCoords:  Coordinates,
    obsCoords:   ObservationTargetsCoordinatesAt,
    wavelength:  Option[Wavelength],
    angles:      NonEmptyList[Angle],
    candidates:  List[GuideStarCandidate]
  )(ctx: AppContext[IO]): IO[Map[AltairMode, AltairParameters]] =
    val configuration = representativeGnirs(wavelength.getOrElse(FallbackWavelength))

    AltairModeRows.TableAltairModes
      .traverseFilter: mode =>
        configuration
          .agsParams(PortDisposition.Side, mode.guideProbe.some, mode.some)
          .flatTraverse: params =>
            ctx.workerClients.ags
              .requestSingle:
                AgsMessage.AgsRequest(
                  targetId,
                  obsTime,
                  constraints,
                  configuration.agsWavelength,
                  baseCoords,
                  // For AGS sky coordinates behave like science coords.
                  obsCoords.scienceCoords ++ obsCoords.skyCoords,
                  obsCoords.blindOffsetCoords,
                  angles,
                  none,
                  none,
                  params,
                  candidates
                )
              .map:
                _.flatMap(_.headOption).flatMap: usable =>
                  AltairControls
                    .guideStarSeparation(baseCoords.some, usable.target.some, obsTime)
                    .flatMap:
                      AltairConfiguration.default(mode).itcParameters(_, usable.target.rBrightness)
          .map(_.tupleLeft(mode))
      .map(_.toMap)
      .handleErrorWith: t =>
        ctx.logger
          .error(t)(s"Error on Altair modes ags calculation ${t.getMessage()}")
          .as(Map.empty)

  /**
   * The Altair parameters of the guide star AGS finds for each Altair mode of the modes table,
   * pending while the search runs. A mode is missing while AGS runs or when no usable star is
   * found, so that its rows are not offered. Unlike `useAgs`, this is silent: it does not touch the
   * AGS state or the guide star selection.
   */
  def useAltairModesAgs(
    obsTargets:             Option[ObservationTargets],
    obsTime:                Option[Instant],
    positions:              ObsPositions,
    obsConf:                ObsConfiguration,
    requirementsWavelength: Option[Wavelength]
  )(ctx: AppContext[IO]): HookResult[Pot[Map[AltairMode, AltairParameters]]] =
    val obsCoords: Option[ObservationTargetsCoordinatesAt] =
      positions.coords.toOption.flatMap(_.toOption)

    // GNIRS rows are only offered where GN is preferred, so only there are Altair rows needed. The
    // search also runs while a mode exists, so its rows are already there when the mode is reverted.
    val enabled: Boolean =
      obsConf.needGuideStar &&
        obsConf.constraints.isDefined &&
        obsCoords.flatMap(_.baseCoords).exists(c => Site.GN.inPreferredDeclination(c.dec))

    for
      candidates     <-
        useEffectResultWithDeps(
          (obsTime.map(SiderealDiscretizedObsTime(_, obsConf.posAngleConstraint)),
           positions.baseTracking,
           obsConf.explicitBase,
           enabled
          )
        ): (discretizedObsTime, oTracking, explicitBase, enabled) =>
          import ctx.given

          // Prefer the explicit base override as the catalog search center
          val searchTracking: Option[Tracking] =
            explicitBase.map(Tracking.constant).orElse(oTracking)

          (discretizedObsTime, searchTracking)
            .mapN: (discretizedObsTime, baseTracking) =>
              CatalogClient[IO]
                .requestSingle:
                  CatalogMessage.GSRequest(
                    baseTracking.minimizeEphemeris(discretizedObsTime.obsTime),
                    discretizedObsTime.obsTime,
                    GuideProbe.AltairAOWFS
                  )
            .filter(_ => enabled)
            .getOrElse(none.pure[IO])
      anglesDebounce <- useDebounce(obsConf.anglesToTest, AgsDebounceDelay.toMillis.toInt)
      _              <- useEffectWithDeps(obsConf.anglesToTest): v =>
                          anglesDebounce.set(v)
      parameters     <-
        useEffectResultWithDeps(
          (obsTargets.map(_.focus.id),
           obsTime,
           obsConf.constraints,
           obsCoords,
           requirementsWavelength,
           anglesDebounce.debouncedValue,
           candidates.value.toOption.flatten,
           enabled
          )
        ):
          case (Some(targetId),
                Some(obsTime),
                Some(constraints),
                Some(obsCoords),
                wavelength,
                angles,
                Some(candidates),
                true
              ) if candidates.nonEmpty =>
            obsCoords.baseCoords.fold(Map.empty.pure[IO]): baseCoords =>
              altairParameters(
                targetId,
                obsTime,
                constraints,
                baseCoords,
                obsCoords,
                wavelength,
                angles.getOrElse(UnconstrainedAngles),
                candidates
              )(ctx)
          case _ =>
            Map.empty.pure[IO]
    // Pending while either search runs: the parameters keep their last value until the candidates
    // arrive, so they cannot tell on their own.
    yield if enabled then candidates.value.value >> parameters.value.value else Pot(Map.empty)
