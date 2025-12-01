// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.implicits.*
import explore.model.syntax.all.*
import lucuma.ags.*
import lucuma.ags.AcquisitionOffsets
import lucuma.ags.syntax.*
import lucuma.core.math.Angle
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength

// Yet another config class. This one is has the minmimal set of params to visualize the configuration
// It is a subset of ObsConfiguration such that can be built out of either the db config
// Or the minimal config from the modes table
case class ConfigurationForVisualization private (
  configuration:              BasicConfiguration,
  scienceOffsets:             Option[NonEmptyList[TelescopeConfig]],
  acquisitionOffsets:         Option[NonEmptyList[TelescopeConfig]],
  selectedPosAngle:           Option[Angle],
  selectedPosAngleConstraint: Option[PosAngleConstraint],
  centralWavelength:          Option[CentralWavelength]
) derives Eq:
  // Effective pos angle, either from the AGS, or the default for the conifguration
  // TODO: Take the calculated average parallactic angle if needed
  val posAngle: Angle =
    selectedPosAngle.getOrElse(
      configuration.obsModeType.defaultPosAngleConstraint.fallbackPosAngle(none)
    )

  def guidedAcqOffsets: Option[NonEmptyList[GuidedOffset]] =
    acquisitionOffsets.flatMap(_.guidedOffsets)

  def asAcqOffsets: Option[AcquisitionOffsets] =
    acquisitionOffsets.flatMap(_.asAcqOffsets)

  def guidedSciOffsets: Option[NonEmptyList[GuidedOffset]] =
    scienceOffsets.flatMap(_.guidedOffsets)

  def asSciOffsets: Option[ScienceOffsets] =
    scienceOffsets.flatMap(_.asSciOffsets)

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
          obsConfig.centralWavelength
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
      basicConfiguration.centralWavelength
    )
