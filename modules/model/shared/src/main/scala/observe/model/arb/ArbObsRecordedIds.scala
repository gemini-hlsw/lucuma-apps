// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.arb

import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.util.arb.ArbGid.given
import lucuma.core.util.arb.ArbNewType.given
import lucuma.core.util.arb.ArbUid.given
import observe.model.*
import observe.model.odb.DatasetIdMap
import observe.model.odb.ObsRecordedIds
import observe.model.odb.RecordedAtom
import observe.model.odb.RecordedAtomId
import observe.model.odb.RecordedStep
import observe.model.odb.RecordedStepId
import observe.model.odb.RecordedVisit
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

import ArbDhsTypes.given

trait ArbObsRecordedIds:
  given Arbitrary[RecordedStep] = Arbitrary:
    for
      stepId     <- arbitrary[RecordedStepId]
      datasetIds <- arbitrary[DatasetIdMap]
    yield RecordedStep(stepId, datasetIds)

  given Arbitrary[RecordedAtom] = Arbitrary:
    for
      generatedId <- arbitrary[Atom.Id]
      atomId      <- arbitrary[RecordedAtomId]
      step        <- arbitrary[Option[RecordedStep]]
    yield RecordedAtom(generatedId, atomId, step)

  given Arbitrary[RecordedVisit] = Arbitrary:
    for
      visitId <- arbitrary[Visit.Id]
      atom    <- arbitrary[Option[RecordedAtom]]
    yield RecordedVisit(visitId, atom)

  given Cogen[RecordedStep] =
    Cogen[(RecordedStepId, DatasetIdMap)].contramap(x => (x.stepId, x.datasetIds))

  given Cogen[RecordedAtom] =
    Cogen[(Atom.Id, RecordedAtomId, Option[RecordedStep])].contramap(x =>
      (x.generatedId, x.atomId, x.step)
    )

  given Cogen[RecordedVisit] =
    Cogen[(Visit.Id, Option[RecordedAtom])].contramap(x => (x.visitId, x.atom))

  given Cogen[ObsRecordedIds] =
    Cogen[List[(Observation.Id, RecordedVisit)]].contramap(_.value.toList)

object ArbObsRecordedIds extends ArbObsRecordedIds
