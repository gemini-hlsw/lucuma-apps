// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptySet
import cats.derived.*
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.enums.AgsState
import explore.model.syntax.all.*
import explore.modes.ConfigSelection
import lucuma.ags.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.TrackType
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.schemas.model.AGSWavelength
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import org.typelevel.cats.time.instances.duration.*

import java.time.Duration

/**
 * Contains, as far as possible, all the configuration for executing an observation, including
 * instrument and telescope config, which includes offsets, duration, etc.
 */
final case class ObsConfiguration(
  configuration:      Option[BasicConfiguration],
  selectedConfig:     ConfigSelection, // selected row(s) in the modes table
  posAngleProperties: Option[PAProperties],
  constraints:        Option[ConstraintSet],
  scienceOffsets:     Option[NonEmptySet[TelescopeConfig]],
  acquisitionOffsets: Option[NonEmptySet[TelescopeConfig]],
  averagePA:          Option[AveragePABasis],
  obsDuration:        Option[Duration],
  needGuideStar:      Boolean,
  remoteGSName:       Option[NonEmptyString],
  calibrationRole:    Option[CalibrationRole],
  trackType:          Option[TrackType]
) derives Eq:

  def agsWavelength: Option[AGSWavelength] =
    configuration.map(_.agsWavelength)

  def conditionsWavelength: Option[Wavelength] =
    configuration.map(_.conditionsWavelength)

  def centralWavelength: Option[CentralWavelength] =
    configuration.flatMap(_.centralWv)

  def posAngleConstraint: Option[PosAngleConstraint] =
    posAngleProperties.map(_.constraint)

  def agsState: Option[View[AgsState]] =
    posAngleProperties.map(_.agsState)

  def guideStarSelection: Option[View[GuideStarSelection]] =
    posAngleProperties.map(_.guideStarSelection)

  // AGS selected position
  def selectedPA: Option[Angle] =
    posAngleProperties.flatMap(_.selectedPA)

  // In case there is no guide star we still want to have a posAngle equivalent
  // To draw visualization
  def fallbackPA: Option[Angle] =
    posAngleProperties.map(_.constraint.fallbackPosAngle(averagePA.map(_.averagePA)))

  def obsModeType: Option[ObservingModeType] =
    configuration.map(_.obsModeType)

  def guideProbe: Option[GuideProbe] =
    configuration.map(_.guideProbe(trackType))

  def guidedAcqOffsets =
    acquisitionOffsets.flatMap(_.asAcqOffsets)

  def guidedSciOffsets =
    scienceOffsets.flatMap(_.asSciOffsets)

object ObsConfiguration:
  def forPlainTarget(
    configuration: Option[BasicConfiguration],
    constraints:   Option[ConstraintSet],
    needsAGS:      Boolean,
    trackType:     Option[TrackType]
  ): ObsConfiguration =
    ObsConfiguration(
      configuration,
      ConfigSelection.Empty,
      none,
      constraints,
      none,
      none,
      none,
      none,
      needsAGS,
      none,
      none,
      trackType
    )
