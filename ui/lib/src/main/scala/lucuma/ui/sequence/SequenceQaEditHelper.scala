// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.effect.IO
import cats.syntax.all.*
import clue.*
import clue.data.syntax.*
import crystal.react.*
import crystal.react.View
import crystal.react.ViewList
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.DatasetQaState
import lucuma.react.primereact.Message
import lucuma.react.primereact.ToastRef
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.tooltip.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.AtomRecord
import lucuma.schemas.model.Dataset
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.StepRecord
import lucuma.schemas.model.Visit
import lucuma.schemas.odb.VisitQueriesGQL
import lucuma.ui.sequence.*
import lucuma.ui.syntax.render.*
import lucuma.ui.syntax.toast.*
import monocle.Optional
import monocle.Traversal
import org.typelevel.log4cats.Logger

// Methods for dealing with dataset QA editing on the sequence table.
trait SequenceQaEditHelper:
  protected def renderQALabel(
    qaState: Option[DatasetQaState],
    comment: Option[NonEmptyString]
  ): String =
    qaState.fold("QA Not Set")(_.shortName) + comment.fold("")(c => s": $c")

  protected def renderQaIcon(
    qaState: Option[DatasetQaState],
    comment: Option[NonEmptyString]
  ): VdomNode =
    <.span(qaState.renderVdom)
      .withTooltip(content = renderQALabel(qaState, comment), position = Tooltip.Position.Top)

  private val gmosNorthDatasets: Traversal[ExecutionVisits, List[Dataset]] =
    ExecutionVisits.gmosNorth
      .andThen(ExecutionVisits.GmosNorth.visits)
      .each
      .andThen(Visit.GmosNorth.atoms)
      .each
      .andThen(AtomRecord.GmosNorth.steps)
      .each
      .andThen(StepRecord.GmosNorth.datasets)

  private val gmosSouthDatasets: Traversal[ExecutionVisits, List[Dataset]] =
    ExecutionVisits.gmosSouth
      .andThen(ExecutionVisits.GmosSouth.visits)
      .each
      .andThen(Visit.GmosSouth.atoms)
      .each
      .andThen(AtomRecord.GmosSouth.steps)
      .each
      .andThen(StepRecord.GmosSouth.datasets)

  private val flamingos2Datasets: Traversal[ExecutionVisits, List[Dataset]] =
    ExecutionVisits.flamingos2
      .andThen(ExecutionVisits.Flamingos2.visits)
      .each
      .andThen(Visit.Flamingos2.atoms)
      .each
      .andThen(AtomRecord.Flamingos2.steps)
      .each
      .andThen(StepRecord.Flamingos2.datasets)

  // This is only lawful if the traverse returns 0 or 1 instances of A.
  private def unsafeHeadOption[T, A](traversal: Traversal[T, A]): Optional[T, A] =
    Optional[T, A](traversal.getAll(_).headOption)(traversal.replace)

  private def instrumentDatasetWithId(traversal: Traversal[ExecutionVisits, List[Dataset]])(
    datasetId: Dataset.Id
  ): Optional[ExecutionVisits, Dataset] =
    unsafeHeadOption(traversal.each.filter(dataset => dataset.id === datasetId))

  private def datasetWithId(datasetId: Dataset.Id): Traversal[ExecutionVisits, Dataset] =
    Traversal.applyN(
      instrumentDatasetWithId(gmosNorthDatasets)(datasetId),
      instrumentDatasetWithId(gmosSouthDatasets)(datasetId),
      instrumentDatasetWithId(flamingos2Datasets)(datasetId)
    )

  private def updateDatasetQa(datasetId: Dataset.Id, qaFields: EditableQaFields)(using
    FetchClient[IO, ObservationDB],
    Logger[IO]
  ): IO[Unit] =
    VisitQueriesGQL
      .UpdateDatasetQa[IO]
      .execute(datasetId, qaFields.qaState.orUnassign, qaFields.comment.orUnassign)
      .void
      .onError:
        case e => Logger[IO].error(e)(s"Error updating dataset QA state for $datasetId")

  protected def onDatasetQaChange(
    visits:             View[Option[ExecutionVisits]],
    datasetIdsInFlight: View[Set[Dataset.Id]],
    toastRef:           ToastRef
  )(using
    FetchClient[IO, ObservationDB],
    Logger[IO]
  ): Dataset.Id => EditableQaFields => Callback =
    datasetId =>
      qaFields =>
        visits.toOptionView.map { visits =>
          def datasetQaView(datasetId: Dataset.Id): ViewList[EditableQaFields] =
            visits.zoom:
              datasetWithId(datasetId).andThen(EditableQaFields.fromDataset)

          datasetIdsInFlight.mod(_ + datasetId) >>
            updateDatasetQa(datasetId, qaFields)
              .flatMap: _ =>
                (datasetQaView(datasetId).set(qaFields) >>
                  datasetIdsInFlight.mod(_ - datasetId))
                  .to[IO]
              .handleErrorWith: e =>
                (datasetIdsInFlight.mod(_ - datasetId) >>
                  toastRef.show(
                    s"Error updating dataset QA state for $datasetId: ${e.getMessage}",
                    Message.Severity.Error,
                    sticky = true
                  )).to[IO]
              .runAsync
        }.orEmpty // If there are no visits, there's nothing to change.
