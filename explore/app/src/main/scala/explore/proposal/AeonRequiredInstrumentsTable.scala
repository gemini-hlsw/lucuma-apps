// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.Order.given
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.Site
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Message
import lucuma.react.table.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import monocle.Lens

// One row per instrument the program can declare required: those used by an observation
// at workflow state Defined or higher. See docs/adr/0008.
case class AeonRequiredInstrumentsTable(
  requiredInstruments: View[Set[Instrument]],
  eligible:            Map[Instrument, Site],
  readonly:            Boolean
) extends ReactFnProps(AeonRequiredInstrumentsTable.component)

object AeonRequiredInstrumentsTable:
  private case class Row(instrument: Instrument, site: Site, required: View[Boolean])

  private case class TableMeta(readonly: Boolean)

  private val ColDef = ColumnDef[Row].WithTableMeta[TableMeta]

  // Membership in the set, as a boolean the checkbox can drive.
  private def memberOf(instrument: Instrument): Lens[Set[Instrument], Boolean] =
    Lens[Set[Instrument], Boolean](_.contains(instrument)): required =>
      set => if required then set + instrument else set - instrument

  private val columns: Reusable[List[ColumnDef.WithTableMeta[Row, ?, TableMeta]]] =
    Reusable.always:
      List(
        ColDef(ColumnId("site"), _.site, "Site", cell = _.value.longName),
        ColDef(ColumnId("instrument"), _.instrument, "Instrument", cell = _.value.longName),
        ColDef(
          ColumnId("required"),
          _.required,
          "Required",
          cell = cell =>
            val instrument = cell.row.original.instrument
            CheckboxView(
              id = NonEmptyString.unsafeFrom(s"aeon-required-${instrument.tag}"),
              value = cell.value,
              label = "",
              disabled = cell.table.options.meta.exists(_.readonly)
            )
        )
      )

  private val component = ScalaFnComponent[AeonRequiredInstrumentsTable]: props =>
    // Sorted up front so the memo key is canonical rather than Map iteration order.
    val eligible: List[(Instrument, Site)] =
      props.eligible.toList.sortBy((instrument, site) => (site, instrument))

    for {
      rows  <- useMemo((props.requiredInstruments.reuseByValue, eligible)):
                 (required, instruments) =>
                   instruments.map: (instrument, site) =>
                     Row(instrument, site, required.value.zoom(memberOf(instrument)))
      table <- useReactTable:
                 TableOptions(
                   columns,
                   rows,
                   getRowId = (row, _, _) => RowId(row.instrument.tag),
                   meta = TableMeta(props.readonly),
                   enableSorting = false,
                   enableColumnResizing = false
                 )
    } yield
      if props.eligible.isEmpty then
        Message(
          text = "No observations with instruments for aeon.",
          severity = Message.Severity.Info
        )
      else
        PrimeTable(
          table,
          striped = true,
          compact = Compact.Very,
          tableMod = ExploreStyles.ExploreBorderTable
        )
