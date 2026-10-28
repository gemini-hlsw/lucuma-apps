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
import explore.model.DaytimeCalibrationSets.Binning
import explore.model.DaytimeCalibrationSets.ReadMode
import explore.model.Flamingos2DaytimeSetup
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
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
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

  private type Row = (DaytimeCalibrationSet, Int)

  private val ColDef = ColumnDef[Row]

  // Not exposed by the MultiSelect binding; reaches PrimeReact as a raw prop.
  private val SelectAllLabel = VdomAttr("selectAllLabel")

  private val DragHandleColumnId: ColumnId  = ColumnId("dragHandle")
  private val KindColumnId: ColumnId        = ColumnId("kind")
  private val CountColumnId: ColumnId       = ColumnId("count")
  private val ExposureColumnId: ColumnId    = ColumnId("exposure")
  private val FpuColumnId: ColumnId         = ColumnId("fpu")
  private val GratingColumnId: ColumnId     = ColumnId("grating")
  private val WavelengthColumnId: ColumnId  = ColumnId("wavelength")
  private val FilterColumnId: ColumnId      = ColumnId("filter")
  private val ReadModeColumnId: ColumnId    = ColumnId("readMode")
  private val BinningColumnId: ColumnId     = ColumnId("binning")
  private val RoiColumnId: ColumnId         = ColumnId("roi")
  private val DeckerColumnId: ColumnId      = ColumnId("decker")
  private val ReadsColumnId: ColumnId       = ColumnId("reads")
  private val WindowCoverColumnId: ColumnId = ColumnId("windowCover")
  private val GcalGroupColumnId: ColumnId   = ColumnId("gcal")
  private val LampColumnId: ColumnId        = ColumnId("lamp")
  private val GcalFilterColumnId: ColumnId  = ColumnId("gcalFilter")
  private val ShutterColumnId: ColumnId     = ColumnId("shutter")
  private val DeleteColumnId: ColumnId      = ColumnId("delete")

  private val ArcGroups: NonEmptyList[(String, GcalArc => Boolean)] =
    NonEmptyList.one(("Arcs", _ => true))

  private val ExposureSeconds =
    formats.durationS.toCodec.withIntegerMask(allowMinus = false)

  private val DefaultGcalLens: Lens[DaytimeCalibrationSet, StepConfig.Gcal] =
    Lens[DaytimeCalibrationSet, StepConfig.Gcal](_.gcal.getOrElse(DefaultGcal))(g =>
      _.copy(gcal = g.some)
    )

  private def cellId(colId: ColumnId, idx: Int): NonEmptyString =
    NonEmptyString.unsafeFrom(s"daytime-cal-set-${colId.value}-$idx")

  private def dropdown[A: Enumerated: Display](
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

  private def optionalDropdown[A: Enumerated: Display](
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

  private def rowView(
    setsView: View[List[DaytimeCalibrationSet]],
    idx:      Int
  ): View[DaytimeCalibrationSet] =
    setsView.zoom(_.apply(idx))(f => l => l.updated(idx, f(l(idx))))

  // Rows always carry the instrument's setup; a mismatch would have been refused by the ODB.
  private def setupView[D](
    setsView: View[List[DaytimeCalibrationSet]],
    setup:    Optional[DaytimeCalibrationSetup, D],
    default:  D
  )(idx: Int): View[D] =
    rowView(setsView, idx).zoom(
      DaytimeCalibrationSet.setup.andThen(
        Lens[DaytimeCalibrationSetup, D](s => setup.getOption(s).getOrElse(default))(setup.replace)
      )
    )

  private def opticsCell(row: Row)(content: => VdomNode): VdomNode =
    if usesOptics(row._1.kind) then content else EmptyVdom

  /** The columns every instrument shares, around the columns of its own setup. */
  private def columns(
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

  /** The columns of one GMOS site, generic in its grating, filter and FPU types. */
  private def gmosColumns[
    D,
    G: Enumerated: Display,
    F: Enumerated: Display,
    U: Enumerated: Display
  ](
    site:     GmosDaytimeSetup[D, G, F, U],
    setsView: View[List[DaytimeCalibrationSet]],
    editable: Boolean,
    units:    WavelengthUnits
  ): List[ColDef.Type] =
    val setup: Int => View[D] = setupView(setsView, site.setup, site.default)

    columns(
      site.instrument,
      setsView,
      editable,
      setup(_).zoom(site.exposure),
      List(
        ColDef(
          FpuColumnId,
          header = "FPU",
          cell = c =>
            opticsCell(c.row.original):
              optionalDropdown(
                FpuColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(site.fpu),
                "None",
                editable
              )
        ).withSize(110.toPx),
        ColDef(
          GratingColumnId,
          header = "Disperser",
          cell = c =>
            opticsCell(c.row.original):
              optionalDropdown(
                GratingColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(site.grating),
                "Mirror",
                editable
              )
        ).withSize(110.toPx),
        ColDef(
          WavelengthColumnId,
          header = s"λ (${units.symbol})",
          cell = c =>
            val idx: Int = c.row.original._2
            val d: D     = setup(idx).get
            // Editable with a disperser; otherwise the filter's wavelength, read-only.
            opticsCell(c.row.original):
              site.grating.get(d) match
                case Some(_) =>
                  FormInputTextView(
                    id = cellId(WavelengthColumnId, idx),
                    value = setup(idx).zoom(d =>
                      site.wavelength.get(d).getOrElse(DefaultWavelength)
                    )(f => site.wavelength.modify(_.map(f))),
                    codec = inputCodecs.wavelength(units),
                    groupClass = SequenceStyles.SequenceInput,
                    disabled = !editable
                  )
                case None    =>
                  site
                    .centralWavelength(d)
                    .fold(EmptyVdom)(w =>
                      inputCodecs.wavelengthValid(units).reverseGet(w): VdomNode
                    )
        ).withSize(80.toPx),
        ColDef(
          FilterColumnId,
          header = "Filter",
          cell = c =>
            opticsCell(c.row.original):
              optionalDropdown(
                FilterColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(site.filter),
                "None",
                editable
              )
        ).withSize(100.toPx),
        ColDef(
          ReadModeColumnId,
          header = "Read Mode",
          cell = c =>
            dropdown(
              ReadModeColumnId,
              c.row.original._2,
              setup(c.row.original._2).zoom(site.readout.andThen(ReadMode.fromCcdMode)),
              editable
            )
        ).withSize(110.toPx),
        ColDef(
          BinningColumnId,
          header = "Binning",
          cell = c =>
            dropdown(
              BinningColumnId,
              c.row.original._2,
              setup(c.row.original._2).zoom(site.readout.andThen(Binning.fromCcdMode)),
              editable
            )
        ).withSize(70.toPx),
        ColDef(
          RoiColumnId,
          header = "ROI",
          cell = c =>
            dropdown(
              RoiColumnId,
              c.row.original._2,
              setup(c.row.original._2).zoom(site.roi),
              editable
            )
        ).withSize(90.toPx)
      )
    )

  private def flamingos2Columns(
    setsView: View[List[DaytimeCalibrationSet]],
    editable: Boolean
  ): List[ColDef.Type] =
    val setup: Int => View[Flamingos2DynamicConfig] =
      setupView(setsView, Flamingos2DaytimeSetup.setup, Flamingos2DaytimeSetup.default)

    columns(
      Instrument.Flamingos2,
      setsView,
      editable,
      setup(_).zoom(Flamingos2DynamicConfig.exposure),
      List(
        ColDef(
          FilterColumnId,
          header = "Filter",
          cell = c =>
            opticsCell(c.row.original):
              dropdown(
                FilterColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(Flamingos2DynamicConfig.filter),
                editable
              )
        ).withSize(90.toPx),
        ColDef(
          GratingColumnId,
          header = "Disperser",
          cell = c =>
            opticsCell(c.row.original):
              optionalDropdown(
                GratingColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(Flamingos2DynamicConfig.disperser),
                "None",
                editable
              )
        ).withSize(100.toPx),
        ColDef(
          FpuColumnId,
          header = "FPU",
          cell = c =>
            opticsCell(c.row.original):
              optionalDropdown(
                FpuColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(Flamingos2DaytimeSetup.fpu),
                "Imaging",
                editable
              )
        ).withSize(110.toPx),
        ColDef(
          DeckerColumnId,
          header = "Decker",
          cell = c =>
            opticsCell(c.row.original):
              dropdown(
                DeckerColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(Flamingos2DynamicConfig.decker),
                editable
              )
        ).withSize(90.toPx),
        ColDef(
          ReadModeColumnId,
          header = "Read Mode",
          cell = c =>
            dropdown(
              ReadModeColumnId,
              c.row.original._2,
              setup(c.row.original._2).zoom(Flamingos2DaytimeSetup.readMode),
              editable
            )
        ).withSize(90.toPx),
        ColDef(
          ReadsColumnId,
          header = "Reads",
          cell = c =>
            dropdown(
              ReadsColumnId,
              c.row.original._2,
              setup(c.row.original._2).zoom(Flamingos2DynamicConfig.reads),
              editable
            )
        ).withSize(70.toPx),
        ColDef(
          WindowCoverColumnId,
          header = "Window Cover",
          cell = c =>
            optionalDropdown(
              WindowCoverColumnId,
              c.row.original._2,
              setup(c.row.original._2).zoom(Flamingos2DynamicConfig.windowCover),
              "Auto",
              editable
            )
        ).withSize(90.toPx)
      )
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
                            gmosColumns(GmosDaytimeSetup.north, props.setsView, editable, units)
                          case Instrument.GmosSouth  =>
                            gmosColumns(GmosDaytimeSetup.south, props.setsView, editable, units)
                          case Instrument.Flamingos2 =>
                            flamingos2Columns(props.setsView, editable)
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
