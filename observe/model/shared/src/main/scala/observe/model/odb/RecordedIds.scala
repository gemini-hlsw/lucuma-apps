// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.odb

import cats.Eq
import cats.derived.*
import cats.syntax.option.*
import io.circe.*
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.NewType
import monocle.Focus
import monocle.Lens
import monocle.Optional
import observe.model.dhs.ImageFileId

object DatasetIdMap extends NewType[Map[ImageFileId, Dataset.Id]]:
  val Empty: DatasetIdMap                                             =
    DatasetIdMap(Map.empty)
  def at(fileId: ImageFileId): Lens[DatasetIdMap, Option[Dataset.Id]] =
    Value.at(fileId)
type DatasetIdMap = DatasetIdMap.Type

given KeyEncoder[ImageFileId] = KeyEncoder.instance(_.value)
given KeyDecoder[ImageFileId] = KeyDecoder.instance(ImageFileId(_).some)

case class RecordedVisit(visitId: Visit.Id, datasetIds: DatasetIdMap = DatasetIdMap.Empty)
    derives Eq,
      Encoder.AsObject,
      Decoder
object RecordedVisit:
  val visitId: Lens[RecordedVisit, Visit.Id]                                      = Focus[RecordedVisit](_.visitId)
  val datasetIds: Lens[RecordedVisit, DatasetIdMap]                               = Focus[RecordedVisit](_.datasetIds)
  def datasetId(fileId: ImageFileId): Optional[RecordedVisit, Option[Dataset.Id]] =
    datasetIds.andThen(DatasetIdMap.at(fileId))

object ObsRecordedIds extends NewType[Map[Observation.Id, RecordedVisit]]:
  val Empty: ObsRecordedIds                                                  =
    ObsRecordedIds(Map.empty)
  def at(obsId: Observation.Id): Lens[ObsRecordedIds, Option[RecordedVisit]] =
    Value.at(obsId)
type ObsRecordedIds = ObsRecordedIds.Type
