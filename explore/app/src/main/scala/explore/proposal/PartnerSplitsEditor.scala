// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.syntax.all.*
import crystal.react.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.components.ui.ExploreStyles
import explore.components.ui.PartnerFlags
import explore.model.PartnerSplit
import explore.model.ProgramUser
import explore.model.enums.Visible
import explore.model.inputCodecs
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Partner
import lucuma.core.model.PartnerLeads
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.Message
import lucuma.react.primereact.SelectItem
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.primereact.FormDropdownOptional
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.utils.*

case class PartnerSplitsEditor(
  show:    Visible,
  splits:  View[List[PartnerSplit]],
  users:   List[ProgramUser],
  closeMe: Callback,
  onSave:  List[PartnerSplit] => Callback
) extends ReactFnProps(PartnerSplitsEditor.component)

object PartnerSplitsEditor:
  private type Props = PartnerSplitsEditor

  // The team is table meta, rather than a dependency of the columns, so that a change to it
  // re-renders the cells without replacing the columns.
  private case class TableMeta(users: List[ProgramUser])

  private val ColDef = ColumnDef[View[PartnerSplit]].WithTableMeta[TableMeta]

  private def save(props: Props) =
    if (addsUpTo100(props.splits.get))
      props.onSave(props.splits.get) >> props.closeMe
    else Callback.empty

  private def footer(props: Props) =
    <.div(
      Message(text = "Must add up to 100", severity = Message.Severity.Error)
        .unless(addsUpTo100(props.splits.get)),
      Button(label = "Cancel", severity = Button.Severity.Danger, onClick = props.closeMe),
      Button(
        label = "OK",
        severity = Button.Severity.Success,
        onClick = save(props),
        disabled = !addsUpTo100(props.splits.get)
      )
    )

  private def makeId(partner: Partner)                    = s"${partner.tag}-percent"
  private def makePartnerCell(partner: Partner): VdomNode =
    <.label(
      <.img(
        ^.src := PartnerFlags.smallFlag(partner),
        ^.alt := s"${partner.shortName} Flag",
        ExploreStyles.PartnerSplitFlag
      ),
      partner.longName,
      ^.htmlFor := makeId(partner)
    )

  private def makePercentCell(split: View[PartnerSplit]) =
    <.span(
      FormInputTextView(
        id = NonEmptyString.from(makeId(split.get.partner)).getOrElse("SPLIT_ID".refined),
        value = split.zoom(PartnerSplit.percent),
        codec = inputCodecs.Percent
      ).withMods(
        ^.autoFocus := split.get.partner === Partner.AR
      )
    )

  private def userName(u: ProgramUser): String =
    Some(u.name).filter(_.trim.nonEmpty).orElse(u.email).getOrElse(u.id.toString)

  // The lead is chosen among the investigators affiliated with the partner.  The PI always leads
  // the PI's own partner, so that row is not editable, and a partner without time has no lead.
  private def makeLeadCell(split: View[PartnerSplit], users: List[ProgramUser]): VdomNode =
    val s          = split.get
    val byId       = users.map(u => u.id -> u).toMap
    val candidates =
      PartnerLeads.candidates(s.partner, users.map(_.leadCandidate)).flatMap(c => byId.get(c.id))

    if s.percent.value === 0 then EmptyVdom
    else
      candidates.find(_.leadCandidate.isPi) match
        case Some(pi)                   => <.span(userName(pi))
        case None if candidates.isEmpty =>
          <.span(ExploreStyles.WarningInput, "No affiliated investigator")
        case None                       =>
          FormDropdownOptional(
            id = NonEmptyString.unsafeFrom(s"${s.partner.tag}-lead"),
            value = s.lead,
            options = candidates.map(u => SelectItem(u.id, userName(u))),
            onChange = split.zoom(PartnerSplit.lead).set,
            placeholder = "Select a lead",
            clazz = ExploreStyles.WarningInput.when_(s.lead.isEmpty)
          )

  private def total(splits:       List[PartnerSplit]) = splits.map(_.percent.value).sum
  private def addsUpTo100(splits: List[PartnerSplit]) = total(splits) === 100

  private val columns: Reusable[List[ColDef.Type]] =
    Reusable.always:
      List(
        ColDef(
          id = ColumnId("partner"),
          accessor = _.get.partner,
          header = "Partner",
          cell = cell => makePartnerCell(cell.value),
          footer = _ => "Total"
        ),
        ColDef(
          id = ColumnId("percent"),
          header = "Percent",
          cell = cell => makePercentCell(cell.row.original),
          footer = ctx =>
            val tot: Int = ctx.table.getRowModel().rows.map(_.original.get.percent.value).sum
            s"${tot}%"
        ),
        ColDef(
          id = ColumnId("lead"),
          header = "Lead",
          cell =
            cell => cell.table.options.meta.map(meta => makeLeadCell(cell.row.original, meta.users))
        )
      )

  protected val component = ScalaFnComponent[Props]: props =>
    // Every edit re-applies the lead rules, so a default lead appears, or a lead goes, as soon as a
    // partner's time changes -- just as the ODB will do when the splits are saved.
    val splits: View[List[PartnerSplit]] =
      props.splits.zoom(identity[List[PartnerSplit]])(f =>
        a => PartnerSplit.withLeads(f(a), props.users)
      )

    for
      // The team can change while the dialog is open, when another user's edit arrives, so the
      // rules are re-applied to the draft then too.  Otherwise OK could send a lead who is no
      // longer eligible, which the ODB rejects.
      _     <- useEffectWithDeps(props.users): users =>
                 props.splits.mod(PartnerSplit.withLeads(_, users))
      rows  <- useMemo((props.splits.get, props.users))(_ => splits.toListOfViews)
      table <- useReactTable:
                 TableOptions(
                   columns,
                   rows,
                   getRowId = (row, _, _) => RowId(row.get.partner.tag),
                   enableSorting = false,
                   enableColumnResizing = false,
                   meta = TableMeta(props.users)
                 )
    yield Dialog(
      visible = props.show.value,
      onHide = props.closeMe,
      header = "PartnerSplits",
      footer = footer(props),
      closable = false,
      resizable = false,
      focusOnShow = true,
      clazz = ExploreStyles.PartnerSplitsEditorDialog
    )(
      PrimeTable(
        table,
        striped = true,
        compact = Compact.Very,
        tableMod = ExploreStyles.ExploreBorderTable
      )
    )
