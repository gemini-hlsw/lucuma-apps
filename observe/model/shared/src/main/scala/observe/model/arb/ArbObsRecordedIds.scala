// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.arb

import lucuma.core.model.Visit
import lucuma.core.util.arb.ArbGid.given
import lucuma.core.util.arb.ArbNewType.given
import observe.model.*
import observe.model.odb.DatasetIdMap
import observe.model.odb.ObsRecordedIds
import observe.model.odb.RecordedVisit
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

import ArbDhsTypes.given

trait ArbObsRecordedIds:
  given Arbitrary[RecordedVisit] = Arbitrary:
    for
      visitId    <- arbitrary[Visit.Id]
      datasetIds <- arbitrary[DatasetIdMap]
    yield RecordedVisit(visitId, datasetIds)

  given Cogen[RecordedVisit] =
    Cogen[(Visit.Id, DatasetIdMap)].contramap(x => (x.visitId, x.datasetIds))

  given Cogen[ObsRecordedIds] =
    Cogen[List[(Observation.Id, RecordedVisit)]].contramap(_.value.toList)

object ArbObsRecordedIds extends ArbObsRecordedIds
