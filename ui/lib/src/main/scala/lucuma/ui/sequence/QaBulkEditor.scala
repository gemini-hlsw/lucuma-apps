// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.hooks.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.OverlayPanel
import lucuma.react.primereact.hooks.all.*
import lucuma.schemas.model.Dataset
import lucuma.ui.primereact.*

case class QaBulkEditor(
  datasets: List[Dataset],
  inFlight: Boolean,
  onChange: QaBulkMode => NonEmptyList[Dataset.Id] => EditableQaFields => Callback
) extends ReactFnProps(QaBulkEditor)

object QaBulkEditor
    extends ReactFnComponent[QaBulkEditor](props =>
      for
        qaFields    <- useStateView(EditableQaFields.Empty)
        panelRef    <- useOverlayPanelRef
        toOverwrite <- useStateView(none[(QaBulkMode, EditableQaFields)])
      yield
        val all: Option[NonEmptyList[Dataset]] = NonEmptyList.fromList(props.datasets)
        val unset: List[Dataset]               = props.datasets.filter(_.qaState.isEmpty)

        // Worked out from the current datasets, so a pending confirmation follows live updates.
        def targets(mode: QaBulkMode): List[Dataset] = mode match
          case QaBulkMode.OnlyUnset => unset
          case QaBulkMode.Overwrite => props.datasets
          case QaBulkMode.Clear     =>
            props.datasets.filter(d => d.qaState.isDefined || d.comment.isDefined)

        def withState(mode: QaBulkMode): Int = targets(mode).count(_.qaState.isDefined)

        def withComment(mode: QaBulkMode, fields: EditableQaFields): Int =
          if mode == QaBulkMode.Clear || fields.comment.isDefined then
            targets(mode).count(_.comment.isDefined)
          else 0

        // The datasets whose QA state or comment the update would replace or remove.
        def overwritten(mode: QaBulkMode, fields: EditableQaFields): Int =
          if mode == QaBulkMode.Clear then targets(mode).size
          else
            targets(mode).count: dataset =>
              dataset.qaState.isDefined || (fields.comment.isDefined && dataset.comment.isDefined)

        def confirmLabel(mode: QaBulkMode, fields: EditableQaFields): String =
          val verb: String = if mode == QaBulkMode.Clear then "Clear" else "Overwrite"
          s"$verb ${DatasetQaBulkEdit.datasetCount(overwritten(mode, fields))}"

        val reset: Callback = qaFields.set(EditableQaFields.Empty)

        def setQa(mode: QaBulkMode, fields: EditableQaFields): Callback =
          NonEmptyList
            .fromList(targets(mode))
            .foldMap(targets => props.onChange(mode)(targets.map(_.id))(fields)) >> reset

        // The panel is hidden before confirming, since clicking the dialog would dismiss it anyway.
        // The fields are kept, so cancelling the confirmation doesn't lose them.
        def submit(mode: QaBulkMode): Callback =
          val fields: EditableQaFields = qaFields.get
          panelRef.hide >>
            (if overwritten(mode, fields) > 0 then toOverwrite.set((mode, fields).some)
             else setQa(mode, fields))

        val noState: Boolean       = qaFields.get.qaState.isEmpty
        // An empty form clears, as deselecting everything does on a single dataset.
        val emptyForm: Boolean     = noState && qaFields.get.comment.isEmpty
        val toClear: List[Dataset] = targets(QaBulkMode.Clear)

        <.span(SequenceStyles.QaBulkEditor)(
          Button(
            icon = SequenceIcons.Pencil,
            label = "Set QA",
            loading = props.inFlight,
            disabled = all.isEmpty,
            tooltip =
              if all.isEmpty then "No completed or stopped steps with datasets"
              else "Set or clear the QA state of the datasets of these steps",
            tooltipOptions = DefaultTooltipOptions,
            // preventDefault rather than stopPropagation: see the header cell in SequenceColumns.
            onClickE = e => e.preventDefaultCB >> panelRef.toggle(e)
          ).mini.veryCompact,
          OverlayPanel()(
            QaFieldsForm(
              qaFields,
              submit(QaBulkMode.OnlyUnset),
              cancel = panelRef.hide >> reset,
              submitDisabled = noState || unset.isEmpty,
              submitLabel = s"Set for unset (${unset.size})",
              extraActions =
                if emptyForm then
                  Button(
                    icon = SequenceIcons.Trash,
                    severity = Button.Severity.Danger,
                    disabled = toClear.isEmpty,
                    tooltip = "Remove the QA state and comment of every dataset",
                    tooltipOptions = DefaultTooltipOptions,
                    onClick = submit(QaBulkMode.Clear)
                  ).veryCompact(s"Clear all (${toClear.size})")
                else
                  Button(
                    icon = SequenceIcons.Check,
                    severity = Button.Severity.Warning,
                    disabled = noState || all.isEmpty,
                    onClick = submit(QaBulkMode.Overwrite)
                  ).veryCompact(s"Set all (${props.datasets.size})")
            )
          ).withMods(SequenceStyles.QaEditorOverlay).withRef(panelRef.ref),
          Dialog(
            header = toOverwrite.get.foldMap[String]:
              case (QaBulkMode.Clear, _) =>
                s"Clear the QA of ${DatasetQaBulkEdit.datasetCount(toClear.size)}?"
              case (mode, fields)        =>
                s"Set ${DatasetQaBulkEdit.datasetCount(targets(mode).size)} to " +
                  s"${fields.qaState.foldMap(_.tag)}?",
            visible = toOverwrite.get.isDefined,
            modal = true,
            onHide = toOverwrite.set(none),
            footer = <.div(
              Button(
                label = "Cancel",
                icon = SequenceIcons.XMark,
                severity = Button.Severity.Secondary,
                onClick = toOverwrite.set(none)
              ).small.compact,
              Button(
                label = toOverwrite.get.foldMap[String](confirmLabel.tupled),
                icon = SequenceIcons.Check,
                severity = Button.Severity.Danger,
                disabled = props.inFlight,
                // Cleared first, so a second click before the dialog closes sends nothing.
                onClick = toOverwrite.set(none) >> toOverwrite.get.foldMap(setQa.tupled)
              ).small.compact
            )
          )(
            toOverwrite.get.map { (mode, fields) =>
              val states: Int   = withState(mode)
              val comments: Int = withComment(mode, fields)
              <.div(
                if mode == QaBulkMode.Clear then "This removes:" else "This replaces:",
                <.ul(
                  <.li(s"the QA state of ${DatasetQaBulkEdit.datasetCount(states)}")
                    .when(states > 0),
                  <.li(s"the comment of ${DatasetQaBulkEdit.datasetCount(comments)}")
                    .when(comments > 0)
                ),
                "Previous values can't be restored in bulk."
              )
            }
          )
        )
    )
