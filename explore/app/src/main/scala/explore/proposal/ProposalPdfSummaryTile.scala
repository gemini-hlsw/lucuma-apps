// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.numeric.NonNegLong
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.TileComponent
import explore.components.TileContents
import explore.components.ui.ExploreStyles
import explore.components.ui.PartnerFlags
import explore.model.AppContext
import explore.model.Attachment
import explore.model.AttachmentList
import explore.model.ProposalSummaries
import explore.model.ProposalSummaryGeneration
import explore.model.ProposalTabTileIds
import explore.model.ProposalType
import explore.model.reusability.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.core.util.Timestamp
import lucuma.core.util.time.format.GppDateFormatter
import lucuma.core.util.time.format.GppTimeTZFormatterWithZone
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.react.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*

final case class ProposalPdfSummaryTile(
  programId:         Program.Id,
  authToken:         NonEmptyString,
  attachments:       View[AttachmentList],
  summaryGeneration: View[ProposalSummaryGeneration],
  proposalType:      Option[ProposalType],
  readOnly:          Boolean
) extends Tile[ProposalPdfSummaryTile](
      id = ProposalTabTileIds.PdfSummaryId.id,
      title = "PDF Summary",
      autoHeight = true,
      autoHeightMinRows = 3
    )(ProposalPdfSummaryTile)

object ProposalPdfSummaryTile
    extends TileComponent[ProposalPdfSummaryTile]((props, _) =>
      type UrlMapKey = (Attachment.Id, Timestamp)
      type UrlMap    = Map[UrlMapKey, Pot[String]]

      extension (a: Attachment) def toMapKey: UrlMapKey = (a.id, a.updatedAt)

      case class TableMeta(urlMap: UrlMap, generation: ProposalSummaryGeneration)

      val ColDef = ColumnDef[Attachment].WithTableMeta[TableMeta]

      val PartnerColumnId   = ColumnId("partner")
      val FileNameColumnId  = ColumnId("fileName")
      val SizeColumnId      = ColumnId("size")
      val GeneratedColumnId = ColumnId("generated")
      val ActionsColumnId   = ColumnId("actions")

      val tableLabelButtonClasses = ProposalAttachmentsTable.tableLabelButtonClasses

      def partnerLabel(partner: Option[Partner]): String =
        partner.fold("Proposal")(_.shortName)

      def partnerCell(partner: Partner): VdomNode =
        <.span(ExploreStyles.ProposalPdfSummaryPartner)(
          <.img(
            ^.src := PartnerFlags.smallFlag(partner),
            ^.alt := s"${partner.shortName} Flag",
            ExploreStyles.PartnerSplitFlag
          ),
          partner.shortName
        )

      // Open the default PDF viewer in the browser.
      def openButton(att: Attachment, urlMap: UrlMap): VdomNode =
        urlMap
          .get(att.toMapKey)
          .foldMap:
            case Pot.Ready(url) =>
              <.a(Icons.FilePdf,
                  ^.href   := url,
                  ^.target := "_blank",
                  ^.rel    := "noopener noreferrer",
                  tableLabelButtonClasses
              )
                .withTooltip("Open in a new tab")
            case Pot.Pending    => <.span(Icons.Spinner.withSpin(true))
            case Pot.Error(t)   => <.span(Icons.ExclamationTriangle).withTooltip(t.getMessage)

      def generatedAt(ts: Timestamp): String =
        val ldt = ts.toLocalDateTime
        s"${GppDateFormatter.format(ldt)} ${GppTimeTZFormatterWithZone.format(ldt)}"

      val columns: List[ColumnDef.WithTableMeta[Attachment, ?, TableMeta]] = List(
        ColDef(PartnerColumnId, _.summaryPartner, "Partner")
          .withCell(_.value.map(partnerCell)),
        ColDef(FileNameColumnId, _.fileName.value, "File Name"),
        ColDef(SizeColumnId, a => NonNegLong.from(a.fileSize).toOption, "Size")
          .withCell(_.value.foldMap(_.toHumanReadableByteCount)),
        ColDef(GeneratedColumnId, identity, "Generated at")
          .withCell: cell =>
            cell.table.options.meta.map: meta =>
              if (meta.generation.isPending)
                <.span(Icons.Spinner.withSpin(true), " Generating...")
              else
                // A failure beside the timestamp means this PDF is the one the render failed to
                // replace.
                <.span(
                  generatedAt(cell.value.updatedAt),
                  meta.generation
                    .failureFor(cell.value.summaryPartner)
                    .map(f =>
                      <.span(
                        " ",
                        Icons.ExclamationTriangle.withClass(ExploreStyles.WarningIcon)
                      ).withTooltip(f.message)
                    )
                ),
        ColDef(ActionsColumnId, identity, "")
          .withCell: cell =>
            cell.table.options.meta.map(meta => openButton(cell.value, meta.urlMap))
      )

      for
        ctx    <- useContext(AppContext.ctx)
        client <- useMemo(props.authToken)(token => OdbRestClient[IO](ctx.odbRestURI, token))
        urlMap <- useStateView[UrlMap](Map.empty)
        cols   <- useMemo(())(_ => columns)
        splits <- useMemo(props.proposalType): pt =>
                    pt.foldMap(ProposalType.anyPartnerSplits.get)
        rows   <- useMemo((props.attachments.reuseByValue, splits)): (v, s) =>
                    ProposalSummaries.of(v.get, s.value)
        _      <- useEffectWithDeps(rows): summaries =>
                    import ctx.given
                    val current = summaries.value.map(_.toMapKey).toSet
                    val added   = current.filterNot(urlMap.get.contains).toList
                    val reset   = urlMap.mod(m =>
                      added.foldLeft(m.filter((k, _) => current.contains(k)))(
                        _.updated(_, Pot.pending)
                      )
                    )
                    val fetch   = added.traverse_ : key =>
                      ProposalAttachmentsTable
                        .getAttachmentUrl(key._1, client)
                        .flatMap(pot => urlMap.mod(_.updated(key, pot)).toAsync)
                    (reset.toAsync *> fetch).runAsync
        table  <- useReactTable(
                    TableOptions(
                      cols,
                      rows,
                      enableSorting = false,
                      getRowId = (row, _, _) => RowId(row.id.toString),
                      meta = TableMeta(urlMap.get, props.summaryGeneration.get)
                    )
                  )
      yield
        import ctx.given

        val generation = props.summaryGeneration.get

        // The ODB commits the job before answering, so the reply already says Pending.
        val regenerate: IO[Unit] =
          ctx.odbApi
            .regenerateProposalSummaries(props.programId)
            .attempt
            .flatMap:
              _.fold(
                t => ToastCtx[IO].showToast(t.getMessage, Message.Severity.Error, true),
                g => props.summaryGeneration.set(g).toAsync
              )

        val title =
          <.span(ExploreStyles.ProposalPdfSummaryTitle)(
            HelpIcon("proposal/main/pdf-summary.md".refined),
            <.span(
              Button(
                severity = Button.Severity.Secondary,
                icon = Icons.ArrowsRotate,
                tooltip =
                  if (generation.isPending) "Regenerate again with the latest changes"
                  else "Regenerate the PDF summary",
                onClick = regenerate.runAsync
              ).tiny.compact
            ).unless(props.readOnly)
          )

        // Failures with no row to hang them on, as after a first-ever render fails.
        val orphanFailures =
          generation.failuresWithout(rows.value.map(_.summaryPartner).toSet)

        val orphanFailureMessages =
          orphanFailures.map: f =>
            <.div(
              Icons.ExclamationTriangle.withClass(ExploreStyles.WarningIcon),
              s" ${partnerLabel(f.partner)}: ${f.message}"
            )

        val emptyMessage =
          if (generation.isPending)
            <.span(Icons.Spinner.withSpin(true), " Generating the PDF summary...")
          else if (orphanFailures.isEmpty) <.span("No PDF summaries yet.")
          else EmptyVdom

        TileContents(
          title = title,
          body = <.div(ExploreStyles.ProposalPdfSummaryTile)(
            if (rows.isEmpty)
              <.div(ExploreStyles.ProposalPdfSummaryEmpty,
                    emptyMessage,
                    orphanFailureMessages.toTagMod
              )
            else
              <.div(
                PrimeTable(
                  table,
                  striped = true,
                  compact = Compact.Very,
                  tableMod = ExploreStyles.AttachmentsTable
                ),
                <.div(ExploreStyles.ProposalPdfSummaryEmpty, orphanFailureMessages.toTagMod)
                  .when(orphanFailures.nonEmpty)
              )
          )
        )
    )
