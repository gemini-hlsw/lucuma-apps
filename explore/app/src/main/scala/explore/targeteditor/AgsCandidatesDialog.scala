// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.Placement
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.typed.tanstackVirtualCore as rawVirtual
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.ColumnSize.*

/**
 * The list icon by the guide star arrows, and the dialog it opens. The dialog lists the usable
 * guide star candidates, best first, so the current star can be compared with the rest and another
 * one picked without stepping through them. Select is enabled for any star except the current
 * manual pick: picking the current automatic star keeps it as a manual pick.
 *
 * `current` is the star in use, `isManual` whether it was picked by hand, and `currentProblem` why
 * it can't be used when it is a manual pick that isn't among the candidates.
 */
case class AgsCandidatesDialog(
  enabled:        Boolean,
  candidates:     List[AgsAnalysis.Usable],
  current:        Option[NonEmptyString],
  isManual:       Boolean,
  currentProblem: Option[String],
  readonly:       Boolean,
  onSelect:       NonEmptyString => Callback
) extends ReactFnProps(AgsCandidatesDialog.component)

object AgsCandidatesDialog:
  private type Props = AgsCandidatesDialog

  private val component =
    ScalaFnComponent[Props]: props =>
      for
        open   <- useStateView(false)
        chosen <- useStateView(props.current)
      yield
        val close: Callback = open.set(false)

        val canSelect: Boolean =
          !props.readonly &&
            chosen.get.exists: name =>
              props.candidates.exists(_.target.name === name) &&
                !(props.isManual && props.current.contains(name))

        val footer: VdomNode =
          <.div(
            Button(
              label = "Select",
              icon = Icons.Checkmark,
              disabled = !canSelect,
              onClick = chosen.get.foldMap(props.onSelect) >> close
            ).small.compact,
            Button(
              label = "Close",
              icon = Icons.Close,
              severity = Button.Severity.Secondary,
              onClick = close
            ).small.compact
          )

        // Only rendered while open, so the table mounts with it and can scroll to the current star
        val dialog: VdomNode =
          if !open.get then EmptyVdom
          else
            Dialog(
              visible = true,
              onHide = close,
              header = "Guide Star Candidates",
              footer = footer,
              position = DialogPosition.Top,
              closeOnEscape = true,
              dismissableMask = true,
              resizable = false,
              clazz = ExploreStyles.AgsCandidatesDialog
            )(
              (props.current, props.currentProblem).mapN: (name, problem) =>
                <.div(
                  ExploreStyles.AgsCandidatesMissing,
                  Icons.ExclamationTriangle,
                  s"${name.value}: $problem"
                ),
              <.div(
                ExploreStyles.AgsCandidatesTable,
                AgsCandidatesTable(props.candidates, props.current, props.isManual, chosen)
              )
            )

        React.Fragment(
          <.span(
            LucumaPrimeStyles.Disabled.unless(props.enabled),
            ^.onClick -->? Option.when(props.enabled)(chosen.set(props.current) >> open.set(true)),
            ExploreStyles.AgsNavigationButton,
            Icons.ListIcon
          ).withTooltip("Show all candidates"),
          dialog
        )

// A separate component so it mounts with the dialog's content, when the table can be scrolled.
private case class AgsCandidatesTable(
  candidates: List[AgsAnalysis.Usable],
  current:    Option[NonEmptyString],
  isManual:   Boolean,
  chosen:     View[Option[NonEmptyString]]
) extends ReactFnProps(AgsCandidatesTable.component)

private object AgsCandidatesTable:
  private type Props = AgsCandidatesTable

  private val ColDef = ColumnDef[AgsAnalysis.Usable]

  private val CurrentColumnId: ColumnId    = ColumnId("current")
  private val NameColumnId: ColumnId       = ColumnId("name")
  private val SpeedColumnId: ColumnId      = ColumnId("speed")
  private val BrightnessColumnId: ColumnId = ColumnId("brightness")

  private val ScrollOptions =
    rawVirtual.mod
      .ScrollToOptions()
      .setBehavior(rawVirtual.mod.ScrollBehavior.auto)
      .setAlign(rawVirtual.mod.ScrollAlignment.center)

  private def columns(
    current:  Option[NonEmptyString],
    isManual: Boolean
  ): List[ColumnDef[AgsAnalysis.Usable, ?, Nothing, Nothing, Nothing, Any, Nothing]] =
    val currentTitle: String =
      if isManual then "Current guide star, picked by hand"
      else "Current guide star, picked automatically"

    List(
      ColDef(
        CurrentColumnId,
        _.target.name,
        "",
        cell =>
          if current.contains(cell.value) then
            <.span(Icons.Bahai).withTooltip(currentTitle, Placement.Right)
          else EmptyVdom,
        enableSorting = false
      ).withColumnSize(FixedSize(30.toPx)),
      ColDef(
        NameColumnId,
        _.target.name.value,
        "Name",
        enableSorting = false
      ).withColumnSize(FixedSize(280.toPx)),
      ColDef(
        SpeedColumnId,
        _.guideSpeed,
        "Speed",
        cell =>
          <.span(
            AgsOverlay.guideSpeedIcon(cell.value),
            <.span(ExploreStyles.AgsGuideSpeed, cell.value.tag)
          ),
        enableSorting = false
      ).withColumnSize(FixedSize(100.toPx)),
      ColDef(
        BrightnessColumnId,
        identity,
        "Brightness",
        cell => AgsOverlay.brightness(cell.value).getOrElse(EmptyVdom),
        enableSorting = false
      ).withColumnSize(FixedSize(120.toPx))
    )

  private val component =
    ScalaFnComponent[Props]: props =>
      for
        cols           <- useMemo((props.current, props.isManual))(columns.tupled)
        rows           <- useMemo(props.candidates.map(_.target.name))(_ => props.candidates)
        table          <- useReactTable:
                            TableOptions(cols, rows, enableSorting = false, enableColumnResizing = false)
        virtualizerRef <- useRef(none[HTMLTableVirtualizer])
        // Opens on the current star
        _              <- useEffectOnMount:
                            props.current
                              .map(name => props.candidates.indexWhere(_.target.name === name))
                              .filter(_ >= 0)
                              .foldMap: i =>
                                virtualizerRef.get.map(_.foreach(_.scrollToIndex(i, ScrollOptions)))
      yield PrimeAutoHeightVirtualizedTable(
        table,
        estimateSize = _ => 32.toPx,
        striped = true,
        compact = Compact.Very,
        tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.ExploreSelectableTable,
        virtualizerRef = virtualizerRef,
        rowMod = rowTagMod: row =>
          val name = row.original.target.name
          TagMod(
            ExploreStyles.TableRowSelected.when(props.chosen.get.contains(name)),
            ExploreStyles.AgsCandidateCurrent.when(props.current.contains(name)),
            ^.cursor.pointer,
            ^.onClick --> props.chosen.set(name.some)
          )
        ,
        emptyMessage = "No guide star candidates"
      )
