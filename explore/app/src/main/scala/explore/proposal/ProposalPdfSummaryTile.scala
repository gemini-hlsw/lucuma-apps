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
import explore.components.Tile
import explore.components.TileComponent
import explore.components.TileContents
import explore.components.ui.ExploreStyles
import explore.components.ui.PartnerFlags
import explore.model.AppContext
import explore.model.Attachment
import explore.model.AttachmentList
import explore.model.ProposalSummaries
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
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.react.table.*
import lucuma.ui.primereact.*
import lucuma.ui.react.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*

import scala.concurrent.duration.*

final case class ProposalPdfSummaryTile(
  programId:    Program.Id,
  authToken:    NonEmptyString,
  attachments:  View[AttachmentList],
  proposalType: Option[ProposalType],
  readOnly:     Boolean
) extends Tile[ProposalPdfSummaryTile](
      id = ProposalTabTileIds.PdfSummaryId.id,
      title = "PDF Summary",
      autoHeight = true,
      autoHeightMinRows = 3
    )(ProposalPdfSummaryTile)

object ProposalPdfSummaryTile
    extends TileComponent[ProposalPdfSummaryTile]((props, _) =>
      // The ODB gives no failure signal, so a request that outlives this is abandoned.
      val RequestTimeout = 2.minutes

      type UrlMapKey = (Attachment.Id, Timestamp)
      type UrlMap    = Map[UrlMapKey, Pot[String]]

      extension (a: Attachment) def toMapKey: UrlMapKey = (a.id, a.updatedAt)

      case class TableMeta(urlMap: UrlMap, request: Option[ProposalSummaries.Request])

      val ColDef = ColumnDef[Attachment].WithTableMeta[TableMeta]

      val PartnerColumnId   = ColumnId("partner")
      val FileNameColumnId  = ColumnId("fileName")
      val SizeColumnId      = ColumnId("size")
      val GeneratedColumnId = ColumnId("generated")
      val ActionsColumnId   = ColumnId("actions")

      val tableLabelButtonClasses = ProposalAttachmentsTable.tableLabelButtonClasses

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
              <.a(Icons.Eye,
                  ^.href   := url,
                  ^.target := "_blank",
                  ^.rel    := "noopener noreferrer",
                  tableLabelButtonClasses
              )
                .withTooltip("Open in a new tab")
            case Pot.Pending    => <.span(Icons.Spinner.withSpin(true))
            case Pot.Error(t)   => <.span(Icons.ExclamationTriangle).withTooltip(t.getMessage)

      val columns: List[ColumnDef.WithTableMeta[Attachment, ?, TableMeta]] = List(
        ColDef(PartnerColumnId, _.summaryPartner, "Partner")
          .withCell(_.value.map(partnerCell)),
        ColDef(FileNameColumnId, _.fileName.value, "File Name"),
        ColDef(SizeColumnId, a => NonNegLong.from(a.fileSize).toOption, "Size")
          .withCell(_.value.foldMap(_.toHumanReadableByteCount)),
        ColDef(GeneratedColumnId, identity, "Generated at")
          .withCell: cell =>
            cell.table.options.meta.map: meta =>
              if (meta.request.exists(_.isPending(cell.value)))
                <.span(Icons.Spinner.withSpin(true), " Generating...")
              else
                <.span(GppDateFormatter.format(cell.value.updatedAt.toLocalDateTime)),
        ColDef(ActionsColumnId, identity, "")
          .withCell: cell =>
            cell.table.options.meta.map(meta => openButton(cell.value, meta.urlMap))
      )

      for
        ctx      <- useContext(AppContext.ctx)
        client   <- useMemo(props.authToken)(token => OdbRestClient[IO](ctx.odbRestURI, token))
        urlMap   <- useStateView[UrlMap](Map.empty)
        request  <- useStateView(none[ProposalSummaries.Request])
        timedOut <- useStateView(false)
        timeout  <- useSingleEffect
        cols     <- useMemo(())(_ => columns)
        splits   <- useMemo(props.proposalType): pt =>
                      pt.foldMap(ProposalType.anyPartnerSplits.get)
        rows     <- useMemo((props.attachments.reuseByValue, splits)): (v, s) =>
                      ProposalSummaries.of(v.get, s.value)
        _        <- useEffectWithDeps(rows): summaries =>
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
                      // A new PDF landing after the timeout is the answer the banner was waiting for.
                      val settle  = timedOut.set(false).when_(added.nonEmpty)
                      (reset.toAsync *> fetch *> settle.toAsync).runAsync
        // Every summary present at request time has been replaced, so the request is done.
        _        <- useEffectWithDeps((request.get, rows.value)): (req, summaries) =>
                      import ctx.given
                      req
                        .filterNot(_.anyPending(summaries))
                        .map(_ => (request.set(none).toAsync *> timeout.cancel).runAsync)
                        .getOrEmpty
        table    <- useReactTable(
                      TableOptions(
                        cols,
                        rows,
                        enableSorting = false,
                        getRowId = (row, _, _) => RowId(row.id.toString),
                        meta = TableMeta(urlMap.get, request.get)
                      )
                    )
      yield
        import ctx.given

        val regenerate: IO[Unit] =
          for
            req    <- IO(ProposalSummaries.Request(rows.value))
            _      <- (request.set(req.some) *> timedOut.set(false)).toAsync
            result <- ctx.odbApi.regenerateProposalSummaries(props.programId).attempt
            _      <- result.fold(
                        t =>
                          request.set(none).toAsync *>
                            ToastCtx[IO].showToast(t.getMessage, Message.Severity.Error, true),
                        _ =>
                          ToastCtx[IO].showToast("PDF summary regeneration requested") *>
                            timeout.submit(
                              IO.sleep(RequestTimeout) *>
                                (request.set(none) *> timedOut.set(true)).toAsync
                            )
                      )
          yield ()

        // Proposal errors do not gate this: whether a proposal can be rendered is the ODB's
        // call, and it answers with a toast.
        val tooltip =
          if (request.get.isDefined) "Generating..."
          else "Regenerate the PDF summary"

        val title =
          // In a span so the button doesn't take up the full width of the title bar.
          <.span(
            Button(
              severity = Button.Severity.Secondary,
              icon = Icons.Gears,
              loading = request.get.isDefined,
              disabled = request.get.isDefined,
              tooltip = tooltip,
              onClick = regenerate.runAsync
            ).tiny.compact
          ).unless(props.readOnly)

        val timeoutMessage = "Still no PDF summary. Try again."

        val emptyMessage =
          if (request.get.isDefined)
            <.span(Icons.Spinner.withSpin(true), " Generating the PDF summary...")
          else if (timedOut.get) <.span(timeoutMessage)
          else <.span("No PDF summaries yet.")

        TileContents(
          title = title,
          body = <.div(ExploreStyles.ProposalPdfSummaryTile)(
            if (rows.isEmpty)
              <.div(ExploreStyles.ProposalPdfSummaryEmpty, emptyMessage)
            else
              <.div(
                PrimeTable(
                  table,
                  striped = true,
                  compact = Compact.Very,
                  tableMod = ExploreStyles.AttachmentsTable
                ),
                <.div(ExploreStyles.ProposalPdfSummaryEmpty, timeoutMessage).when(timedOut.get)
              )
          )
        )
    )
