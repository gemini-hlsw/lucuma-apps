// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.DaytimeCalibrationSets
import explore.model.GmosDaytimeSetup
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.model.formats
import explore.model.inputCodecs
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationSetType
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.StepConfig
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.DaytimeCalibrationSet
import lucuma.schemas.model.DaytimeCalibrationSetup
import lucuma.ui.dnd.*
import lucuma.ui.input.toCodec
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.sequence.SequenceIcons
import lucuma.ui.sequence.SequenceStyles
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import monocle.Lens
import monocle.Optional

import scala.collection.immutable.SortedSet

/**
 * The Daytime Calibration Sets of a GMOS or Flamingos 2 daytime calibration, one editable row per
 * set, styled and reordered like the sequence table.
 */
case class DaytimeCalibrationSetsTable(
  instrument: Instrument,
  setsView:   View[List[DaytimeCalibrationSet]],
  editable:   Boolean,
  units:      WavelengthUnits
) extends ReactFnProps(DaytimeCalibrationSetsTable.component)

object DaytimeCalibrationSetsTable:
  import DaytimeCalibrationSets.*

  private given Reusability[DaytimeCalibrationSet] = Reusability.byEq

  private[config] type Row = (DaytimeCalibrationSet, Int)

  private[config] val ColDef = ColumnDef[Row]

  // Not exposed by the MultiSelect binding; reaches PrimeReact as a raw prop.
  private val SelectAllLabel = VdomAttr("selectAllLabel")

  private val DragHandleColumnId: ColumnId       = ColumnId("dragHandle")
  private val KindColumnId: ColumnId             = ColumnId("kind")
  private val CountColumnId: ColumnId            = ColumnId("count")
  private val ExposureColumnId: ColumnId         = ColumnId("exposure")
  private[config] val FpuColumnId: ColumnId      = ColumnId("fpu")
  private[config] val GratingColumnId: ColumnId  = ColumnId("grating")
  private[config] val FilterColumnId: ColumnId   = ColumnId("filter")
  private[config] val ReadModeColumnId: ColumnId = ColumnId("readMode")
  private val GcalGroupColumnId: ColumnId        = ColumnId("gcal")
  private val LampColumnId: ColumnId             = ColumnId("lamp")
  private val GcalFilterColumnId: ColumnId       = ColumnId("gcalFilter")
  private val ShutterColumnId: ColumnId          = ColumnId("shutter")
  private val DeleteColumnId: ColumnId           = ColumnId("delete")

  private val ArcGroups: NonEmptyList[(String, GcalArc => Boolean)] =
    NonEmptyList.one(("Arcs", _ => true))

  private val ExposureSeconds =
    formats.durationS.toCodec.withIntegerMask(allowMinus = false)

  private val DefaultGcalLens: Lens[DaytimeCalibrationSet, StepConfig.Gcal] =
    Lens[DaytimeCalibrationSet, StepConfig.Gcal](s =>
      s.gcal.getOrElse(defaultGcal(s.setup.instrument))
    )(g => _.copy(gcal = g.some))

  private[config] def cellId(colId: ColumnId, idx: Int): NonEmptyString =
    NonEmptyString.unsafeFrom(s"daytime-cal-set-${colId.value}-$idx")

  private[config] def dropdown[A: Enumerated: Display](
    colId:    ColumnId,
    idx:      Int,
    value:    View[A],
    editable: Boolean,
    exclude:  Set[A] = Set.empty[A]
  ): VdomNode =
    EnumDropdownView(
      id = cellId(colId, idx),
      value = value,
      exclude = exclude,
      clazz = SequenceStyles.SequenceDropdown,
      panelClass = SequenceStyles.SequenceDropdownPanel,
      disabled = !editable
    )

  private[config] def optionalDropdown[A: Enumerated: Display](
    colId:       ColumnId,
    idx:         Int,
    value:       View[Option[A]],
    placeholder: String,
    editable:    Boolean
  ): VdomNode =
    EnumDropdownOptionalView(
      id = cellId(colId, idx),
      value = value,
      placeholder = placeholder,
      clazz = SequenceStyles.SequenceDropdown,
      panelClass = SequenceStyles.SequenceDropdownPanel,
      disabled = !editable
    )

  private[config] def rowView(
    setsView: View[List[DaytimeCalibrationSet]],
    idx:      Int
  ): View[DaytimeCalibrationSet] =
    setsView.zoom(_.apply(idx))(f => l => l.updated(idx, f(l(idx))))

  // Rows always carry the instrument's setup; a mismatch would have been refused by the ODB.
  private[config] def setupView[D](
    setsView: View[List[DaytimeCalibrationSet]],
    setup:    Optional[DaytimeCalibrationSetup, D],
    default:  D
  )(idx: Int): View[D] =
    rowView(setsView, idx).zoom(
      DaytimeCalibrationSet.setup.andThen(
        Lens[DaytimeCalibrationSetup, D](s => setup.getOption(s).getOrElse(default))(setup.replace)
      )
    )

  private[config] def opticsCell(row: Row)(content: => VdomNode): VdomNode =
    if usesOptics(row._1.kind) then content else EmptyVdom

  /** The columns every instrument shares, around the columns of its own setup. */
  private[config] def columns(
    instrument:   Instrument,
    setsView:     View[List[DaytimeCalibrationSet]],
    editable:     Boolean,
    exposureView: Int => View[TimeSpan],
    setupColumns: List[ColDef.Type]
  ): List[ColDef.Type] =
    def gcalView(idx: Int): View[StepConfig.Gcal] = rowView(setsView, idx).zoom(DefaultGcalLens)

    val excludedKinds: Set[CalibrationSetType] =
      Enumerated[CalibrationSetType].all.toSet -- kinds(instrument)

    List(
      ColDef(
        DragHandleColumnId,
        header = "",
        cell =
          _ => <.span(SequenceStyles.DragHandleCell)(SequenceIcons.GripDotsVertical.when(editable)),
        enableResizing = false
      ).withSize(25.toPx),
      ColDef(
        KindColumnId,
        header = "Type",
        cell = c =>
          dropdown(
            KindColumnId,
            c.row.original._2,
            rowView(setsView, c.row.original._2).zoom(_.kind)(f => s => withKind(s, f(s.kind))),
            editable,
            excludedKinds
          )
      ).withSize(90.toPx),
      ColDef(
        CountColumnId,
        header = "Num Exp",
        cell = c =>
          FormInputTextView(
            id = cellId(CountColumnId, c.row.original._2),
            value = rowView(setsView, c.row.original._2).zoom(DaytimeCalibrationSet.count),
            codec = inputCodecs.CalibrationSetCount,
            groupClass = SequenceStyles.SequenceInput,
            disabled = !editable
          )
      ).withSize(60.toPx),
      ColDef(
        ExposureColumnId,
        header = "Exp Time (s)",
        cell = c =>
          if usesExposure(c.row.original._1.kind) then
            FormInputTextView(
              id = cellId(ExposureColumnId, c.row.original._2),
              value = exposureView(c.row.original._2),
              codec = ExposureSeconds,
              groupClass = SequenceStyles.SequenceInput,
              disabled = !editable
            )
          else EmptyVdom
      ).withSize(70.toPx)
    ) ++ setupColumns ++ List(
      ColDef.group(
        GcalGroupColumnId,
        header = "GCAL",
        columns = List(
          ColDef(
            LampColumnId,
            header = "Lamp",
            cell = c =>
              val idx: Int = c.row.original._2
              c.row.original._1.kind match
                case CalibrationSetType.Flat =>
                  dropdown(
                    LampColumnId,
                    idx,
                    gcalView(idx).zoom(_.lamp.continuum.getOrElse(DefaultContinuum))(f =>
                      g =>
                        g.copy(lamp =
                          StepConfig.Gcal.Lamp
                            .fromContinuum(f(g.lamp.continuum.getOrElse(DefaultContinuum)))
                        )
                    ),
                    editable
                  )
                case CalibrationSetType.Arc  =>
                  // An arc keeps at least one lamp: clearing the last one is ignored.
                  EnumGroupedMultiSelectView(
                    id = cellId(LampColumnId, idx),
                    value = gcalView(idx).zoom(_.lamp.arcs.foldMap(_.toSortedSet.toList))(f =>
                      g =>
                        val current = g.lamp.arcs.getOrElse(DefaultArcs)
                        NonEmptySet
                          .fromSet(
                            SortedSet.from(f(current.toSortedSet.toList))(using
                              Enumerated[GcalArc].toOrdering
                            )
                          )
                          .fold(g)(as => g.copy(lamp = StepConfig.Gcal.Lamp.fromArcs(as)))
                    ),
                    groupFunctions = ArcGroups,
                    clazz = SequenceStyles.SequenceDropdown,
                    panelClass = SequenceStyles.SequenceDropdownPanel,
                    disabled = !editable
                  ).withMods(SelectAllLabel := "All")
                case _                       => EmptyVdom
          ).withSize(110.toPx),
          ColDef(
            GcalFilterColumnId,
            header = "Filter",
            cell = c =>
              val idx: Int = c.row.original._2
              if c.row.original._1.kind.usesGcal then
                dropdown(
                  GcalFilterColumnId,
                  idx,
                  gcalView(idx).zoom(StepConfig.Gcal.filter),
                  editable
                )
              else EmptyVdom
          ).withSize(70.toPx),
          ColDef(
            ShutterColumnId,
            header = "Shutter",
            cell = c =>
              val idx: Int = c.row.original._2
              if c.row.original._1.kind.usesGcal then
                dropdown(ShutterColumnId,
                         idx,
                         gcalView(idx).zoom(StepConfig.Gcal.shutter),
                         editable
                )
              else EmptyVdom
          ).withSize(80.toPx)
        )
      ),
      ColDef(
        DeleteColumnId,
        header = "",
        cell = c =>
          val idx: Int = c.row.original._2
          Button(
            icon = Icons.Trash,
            clazz = SequenceStyles.DeleteButton,
            text = true,
            disabled = !editable,
            tooltip = "Remove this calibration set",
            onClick = setsView.mod(l => l.take(idx) ++ l.drop(idx + 1))
          ).tiny.compact
        ,
        enableResizing = false
      ).withSize(30.toPx)
    )

  private val component =
    ScalaFnComponent[DaytimeCalibrationSetsTable]: props =>
      for
        rows     <- useMemo(props.setsView.get)(_.zipWithIndex)
        // The cells close over the views, so the columns follow every change of the list.
        cols     <- useMemo((props.instrument, props.setsView.get, props.editable, props.units)):
                      (instrument, _, editable, units) =>
                        instrument match
                          case Instrument.GmosNorth  =>
                            GmosDaytimeCalibrationColumns(GmosDaytimeSetup.north,
                                                          props.setsView,
                                                          editable,
                                                          units
                            )
                          case Instrument.GmosSouth  =>
                            GmosDaytimeCalibrationColumns(GmosDaytimeSetup.south,
                                                          props.setsView,
                                                          editable,
                                                          units
                            )
                          case Instrument.Flamingos2 =>
                            Flamingos2DaytimeCalibrationColumns(props.setsView, editable)
                          case _                     => Nil
        table    <- useReactTable:
                      TableOptions(
                        cols,
                        rows,
                        enableSorting = false,
                        enableColumnResizing = true,
                        columnResizeMode = ColumnResizeMode.OnChange,
                        getRowId = (row, _, _) => RowId(row._2.toString)
                      )
        tableDnd <- useTableDragAndDrop(
                      table,
                      DragHandleColumnId,
                      getData = _.original._2,
                      canDrag = (_, _) => props.editable,
                      onDrop = (from, target) =>
                        target.fold(Callback.empty): t =>
                          props.setsView.mod(moveInList(from, t.data, t.edge))
                    )
      yield tableDnd.context(
        <.div(
          PrimeTable(
            table,
            compact = Compact.Very,
            hoverableRows = true,
            celled = true,
            tableMod = SequenceStyles.SequenceTable |+| ExploreStyles.DaytimeCalSetTable,
            headerCellMod = _.column.id match
              case id if id == DragHandleColumnId || id == DeleteColumnId =>
                SequenceStyles.HiddenColTableHeader
              case _                                                      => TagMod.empty,
            rowMod = tableDnd.rowMod(),
            cellMod = tableDnd.cellMod: (cell, _) =>
              cell.column.id match
                case id if id == DragHandleColumnId || id == DeleteColumnId =>
                  TagMod(^.paddingLeft := "0", ^.paddingRight := "0")
                case _                                                      => TagMod.empty,
            emptyMessage = <.div(
              ExploreStyles.DaytimeCalMessage,
              "No calibration sets."
            )
          ),
          Button(
            icon = Icons.ThinPlus,
            severity = Button.Severity.Success,
            text = true,
            label = "calibration set",
            disabled = !props.editable,
            onClick = props.setsView.mod(l => l ++ newRow(l, props.instrument).toList)
          ).tiny.compact
        )
      )
