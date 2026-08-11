// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model.arb

import cats.Order.given
import eu.timepit.refined.scalacheck.string.given
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Attachment
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.arb.ArbConstraintSet.given
import lucuma.core.model.arb.ArbObservationReference.given
import lucuma.core.model.arb.ArbPosAngleConstraint.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.arb.ArbBasicConfiguration.given
import observe.ui.model.ObsSummary
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

trait ArbObsSummary:
  given Arbitrary[ObsSummary] = Arbitrary:
    for
      obsId              <- arbitrary[Observation.Id]
      programId          <- arbitrary[Program.Id]
      title              <- arbitrary[String]
      subtitle           <- arbitrary[Option[NonEmptyString]]
      instrument         <- arbitrary[Instrument]
      constraints        <- arbitrary[ConstraintSet]
      attachmentIds      <- arbitrary[List[Attachment.Id]]
      maskNames          <- arbitrary[List[(Attachment.Id, NonEmptyString)]]
      observingMode      <- arbitrary[Option[BasicConfiguration]]
      observationTime    <- arbitrary[Option[Instant]]
      calRole            <- arbitrary[Option[CalibrationRole]]
      posAngleConstraint <- arbitrary[PosAngleConstraint]
      obsReference       <- arbitrary[Option[ObservationReference]]
      workflowState      <- arbitrary[ObservationWorkflowState]
    yield ObsSummary(
      obsId,
      programId,
      title,
      subtitle,
      instrument,
      constraints,
      SortedSet.from(attachmentIds),
      SortedMap.from(maskNames),
      observingMode,
      observationTime,
      calRole,
      posAngleConstraint,
      obsReference,
      workflowState
    )

  given Cogen[ObsSummary] =
    Cogen[
      (Observation.Id,
       Program.Id,
       String,
       Option[String],
       Instrument,
       ConstraintSet,
       List[Attachment.Id],
       List[(Attachment.Id, String)],
       Option[BasicConfiguration],
       Option[Instant],
       Option[CalibrationRole],
       PosAngleConstraint,
       Option[ObservationReference],
       ObservationWorkflowState
      )
    ]
      .contramap: s =>
        (s.obsId,
         s.programId,
         s.title,
         s.subtitle.map(_.value),
         s.instrument,
         s.constraints,
         s.attachmentIds.toList,
         s.maskNames.view.mapValues(_.value).toList,
         s.observingMode,
         s.observationTime,
         s.calibrationRole,
         s.posAngleConstraint,
         s.obsReference,
         s.workflowState
        )

object ArbObsSummary extends ArbObsSummary
