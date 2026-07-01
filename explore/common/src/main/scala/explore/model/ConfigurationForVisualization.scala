// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptySet
import cats.derived.*
import cats.implicits.*
import explore.model.syntax.all.*
import lucuma.ags.*
import lucuma.ags.AcquisitionOffsets
import lucuma.ags.syntax.*
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.TrackType
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.schemas.model.AGSWavelength
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.InstrumentSlot
import lucuma.schemas.model.SlotId
import lucuma.schemas.model.TargetVisualization

// Yet another config class. This one is has the minmimal set of params to visualize the configuration
// It is a subset of ObsConfiguration such that can be built out of either the db config
// Or the minimal config from the modes table
case class ConfigurationForVisualization private (
  configuration:              BasicConfiguration,
  scienceOffsets:             Option[NonEmptySet[TelescopeConfig]],
  acquisitionOffsets:         Option[NonEmptySet[TelescopeConfig]],
  selectedPosAngle:           Option[Angle],
  selectedPosAngleConstraint: Option[PosAngleConstraint],
  trackType:                  Option[TrackType],
  targetVisualization:        TargetVisualization
) derives Eq:
  // Effective pos angle, either from the AGS, or the default for the conifguration
  // TODO: Take the calculated average parallactic angle if needed
  val posAngle: Angle =
    selectedPosAngle.getOrElse(
      configuration.obsModeType.defaultPosAngleConstraint.fallbackPosAngle(none)
    )

  def guidedAcqOffsets: Option[NonEmptySet[GuidedOffset]] =
    acquisitionOffsets.flatMap(_.guidedOffsets)

  def asAcqOffsets: Option[AcquisitionOffsets] =
    acquisitionOffsets.flatMap(_.asAcqOffsets)

  def guidedSciOffsets: Option[NonEmptySet[GuidedOffset]] =
    scienceOffsets.flatMap(_.guidedOffsets)

  def asSciOffsets: Option[ScienceOffsets] =
    scienceOffsets.flatMap(_.asSciOffsets)

  def agsWavelength: AGSWavelength =
    configuration.agsWavelength

  def guideProbe: Option[GuideProbe] =
    configuration.guideProbe(trackType)

  def conditionsWavelength: Wavelength =
    configuration.conditionsWavelength

  // Whether IFU2 is free to hold a sky position, i.e. no science target is mapped to it.
  // Standard resolution can be one or two targets, so it qualifies only when IFU2 is not used
  // by a science target.
  // High resolution is always a single science target on IFU1 with IFU2 reserved for the sky.
  def isIfu2AvailableForSky: Boolean =
    configuration match
      case _: BasicConfiguration.GhostIfu =>
        !targetVisualization.slots.exists:
          case InstrumentSlot.Science(_, SlotId.GhostIfu2) => true
          case _                                           => false
      case _                              => false

object ConfigurationForVisualization:
  def fromObsConfiguration(
    obsConfig: ObsConfiguration
  ): Option[ConfigurationForVisualization] =
    obsConfig.configuration
      .map { c =>
        ConfigurationForVisualization(
          c,
          obsConfig.scienceOffsets,
          obsConfig.acquisitionOffsets,
          obsConfig.selectedPA.orElse(obsConfig.fallbackPA),
          obsConfig.posAngleConstraint,
          obsConfig.trackType,
          obsConfig.targetViz
        )
      }
      .orElse:
        obsConfig.selectedConfig
          .toBasicConfiguration(withFallbackWavelength = true)
          .map:
            fromBasicConfiguration(_, obsConfig.fallbackPA)

  def fromBasicConfiguration(
    basicConfiguration: BasicConfiguration,
    selectedPosAngle:   Option[Angle]
  ): ConfigurationForVisualization =
    ConfigurationForVisualization(
      basicConfiguration,
      None,
      None,
      selectedPosAngle,
      None,
      None,
      TargetVisualization.Empty
    )
