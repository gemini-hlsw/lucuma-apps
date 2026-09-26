// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.Order.given
import crystal.Pot
import eu.timepit.refined.scalacheck.numeric.given
import eu.timepit.refined.scalacheck.string.given
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.BlindOffset
import explore.model.Execution
import explore.model.Observation
import explore.model.SchedulingConstraints
import explore.model.ScienceRequirements
import explore.model.arb.ArbExecution
import lucuma.core.arb.ArbTime
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.CassRotator
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservationPriority
import lucuma.core.enums.ScienceBand
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbCoordinates.given
import lucuma.core.math.arb.ArbWavelength.given
import lucuma.core.model.Attachment
import lucuma.core.model.Configuration
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Group
import lucuma.core.model.ObservationReference
import lucuma.core.model.ObservationValidation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbConfiguration.given
import lucuma.core.model.arb.ArbConstraintSet.given
import lucuma.core.model.arb.ArbObservationReference.given
import lucuma.core.model.arb.ArbObservationValidation.given
import lucuma.core.model.arb.ArbObservationWorkflow.given
import lucuma.core.model.arb.ArbPosAngleConstraint.given
import lucuma.core.util.CalculatedValue
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbCalculatedValue.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.core.util.arb.ArbTimeSpan.given
import lucuma.odb.data.AltairConfiguration
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.arb.ArbObservingMode
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

import java.time.Instant
import scala.collection.immutable.SortedSet

trait ArbObservation:
  import ArbBlindOffset.given
  import ArbExecution.given
  import ArbTime.given
  import ArbSchedulingConstraints.given
  import ArbScienceRequirements.given
  import ArbObservingMode.given
  import ArbAltairConfiguration.given

  given Arbitrary[Observation] =
    Arbitrary(
      for
        id                    <- arbitrary[Observation.Id]
        reference             <- arbitrary[Option[ObservationReference]]
        title                 <- arbitrary[String]
        subtitle              <- arbitrary[Option[NonEmptyString]]
        scienceTargetIds      <- arbitrary[Set[Target.Id]]
        selectedGSName        <- arbitrary[Option[NonEmptyString]]
        constraints           <- arbitrary[ConstraintSet]
        schedulingConstraints <- arbitrary[SchedulingConstraints]
        attachmentIds         <- arbitrary[Set[Attachment.Id]]
        scienceRequirements   <- arbitrary[ScienceRequirements]
        observingMode         <- arbitrary[Option[ObservingMode]]
        vizTime               <- arbitrary[Option[Instant]]
        vizDuration           <- arbitrary[Option[TimeSpan]]
        posAngleConstraint    <- arbitrary[PosAngleConstraint]
        centralWavelength     <- arbitrary[Option[Wavelength]].map(_.map(CentralWavelength.apply))
        validations           <- arbitrary[List[ObservationValidation]]
        observerNotes         <- arbitrary[Option[NonEmptyString]]
        calibrationRole       <- arbitrary[Option[CalibrationRole]]
        scienceBand           <- arbitrary[Option[ScienceBand]]
        priority              <- arbitrary[ObservationPriority]
        configuration         <- arbitrary[Option[Configuration]]
        crIds                 <- arbitrary[Set[ConfigurationRequest.Id]]
        workflow              <- arbitrary[CalculatedValue[ObservationWorkflow]]
        groupId               <- arbitrary[Option[Group.Id]]
        groupIndex            <- arbitrary[NonNegShort]
        execution             <- arbitrary[Execution]
        explicitBase          <- arbitrary[Option[Coordinates]]
        blindOffset           <- arbitrary[BlindOffset]
        cassRotator           <- arbitrary[CassRotator]
        explicitGuideProbe    <- arbitrary[Option[GuideProbe]]
        altair                <- arbitrary[Option[AltairConfiguration]]
      yield Observation(
        id,
        reference,
        title,
        subtitle,
        SortedSet.from(scienceTargetIds),
        selectedGSName,
        constraints,
        schedulingConstraints,
        SortedSet.from(attachmentIds),
        scienceRequirements,
        observingMode.map(_.toBasicConfiguration),
        Pot.Ready(observingMode),
        vizTime,
        vizDuration,
        posAngleConstraint,
        centralWavelength,
        observerNotes,
        calibrationRole,
        scienceBand,
        priority,
        configuration,
        SortedSet.from(crIds),
        workflow,
        groupId,
        groupIndex,
        execution,
        explicitBase,
        blindOffset,
        cassRotator,
        explicitGuideProbe,
        altair
      )
    )

  given Cogen[Observation] =
    Cogen[
      (Observation.Id,
       Option[ObservationReference],
       String,
       Option[String],
       List[Target.Id],
       Option[String],
       ConstraintSet,
       SchedulingConstraints,
       SortedSet[Attachment.Id],
       ScienceRequirements,
       Option[ObservingMode],
       Option[Instant],
       Option[TimeSpan],
       PosAngleConstraint,
       Option[Wavelength],
       Option[String],
       Option[CalibrationRole],
       Option[ScienceBand],
       Option[Configuration],
       SortedSet[ConfigurationRequest.Id],
       CalculatedValue[ObservationWorkflow],
       (ObservationPriority,
        Option[Group.Id],
        Short,
        Execution,
        Option[Coordinates],
        BlindOffset,
        Option[GuideProbe],
        Option[AltairConfiguration]
       )
      )
    ]
      .contramap(o =>
        (o.id,
         o.reference,
         o.title,
         o.subtitle.map(_.value),
         o.scienceTargetIds.toList,
         o.selectedGSName.map(_.value),
         o.constraints,
         o.schedulingConstraints,
         o.attachmentIds,
         o.scienceRequirements,
         o.observingMode.toOption.flatten,
         o.observationTime,
         o.observationDuration,
         o.posAngleConstraint,
         o.centralWavelength.map(_.value),
         o.observerNotes.map(_.value),
         o.calibrationRole,
         o.scienceBand,
         o.configuration,
         o.configurationRequestIds,
         o.workflow,
         (o.priority,
          o.groupId,
          o.groupIndex.value,
          o.execution,
          o.explicitBase,
          o.blindOffset,
          o.explicitGuideProbe,
          o.altair
         )
        )
      )

object ArbObservation extends ArbObservation
