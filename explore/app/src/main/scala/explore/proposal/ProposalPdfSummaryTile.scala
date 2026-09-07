// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.Tile
import explore.components.TileComponent
import explore.components.TileContents
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Attachment
import explore.model.AttachmentList
import explore.model.ProposalSummaries
import explore.model.ProposalTabTileIds
import explore.model.reusability.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
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
  programId:         Program.Id,
  authToken:         NonEmptyString,
  attachments:       View[AttachmentList],
  readOnly:          Boolean,
  hasProposalErrors: Boolean
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
      val GeneratedColumnId = ColumnId("generated")
      val ActionsColumnId   = ColumnId("actions")

      val tableLabelButtonClasses = ProposalAttachmentsTable.tableLabelButtonClasses

      def linkButtons(att: Attachment, urlMap: UrlMap): VdomNode =
        urlMap
          .get(att.toMapKey)
          .foldMap:
            case Pot.Ready(url) =>
              React.Fragment(
                <.a(Icons.Eye, ^.href := url, ^.target := "_blank", tableLabelButtonClasses)
                  .withTooltip("Open in a new tab"),
                <.a(Icons.FileArrowDown, ^.href := url, ^.download := att.fileName.value)
                  .apply(tableLabelButtonClasses)
                  .withTooltip("Download")
              )
            case Pot.Pending    => <.span(Icons.Spinner.withSpin(true))
            case Pot.Error(t)   => <.span(Icons.ExclamationTriangle).withTooltip(t.getMessage)

      val columns: List[ColumnDef.WithTableMeta[Attachment, ?, TableMeta]] = List(
        ColDef(PartnerColumnId, _.summaryPartner, "Partner")
          .withCell(_.value.foldMap(_.shortName)),
        ColDef(GeneratedColumnId, identity, "Generated at")
          .withCell: cell =>
            cell.table.options.meta.map: meta =>
              if (meta.request.exists(_.isPending(cell.value)))
                <.span(Icons.Spinner.withSpin(true), " Generating...")
              else
                <.span(GppDateFormatter.format(cell.value.updatedAt.toLocalDateTime)),
        ColDef(ActionsColumnId, identity, "")
          .withCell: cell =>
            cell.table.options.meta.map(meta => linkButtons(cell.value, meta.urlMap))
      )

      for
        ctx      <- useContext(AppContext.ctx)
        client   <- useMemo(props.authToken)(token => OdbRestClient[IO](ctx.odbRestURI, token))
        urlMap   <- useStateView[UrlMap](Map.empty)
        request  <- useStateView(none[ProposalSummaries.Request])
        timedOut <- useStateView(false)
        timeout  <- useSingleEffect
        cols     <- useMemo(())(_ => columns)
        rows     <- useMemo(props.attachments.reuseByValue)(v => ProposalSummaries.of(v.get))
        _        <- useEffectWithDeps(rows): summaries =>
                      import ctx.given
                      val current = summaries.value.map(_.toMapKey).toSet
                      val added   = current.filterNot(urlMap.get.contains).toList
                      val reset   = urlMap.mod(m =>
                        added.foldLeft(m.filter((k, _) => current.contains(k)))(
                          _.updated(_, Pot.pending)
                        )
                      )
                      val fetch   = added.traverse_(key =>
                        ProposalAttachmentsTable
                          .getAttachmentUrl(key._1, client)
                          .flatMap(pot => urlMap.mod(_.updated(key, pot)).toAsync)
                      )
                      // A new PDF landing after the timeout is the answer the banner was waiting for.
                      val settle  = timedOut.set(false).when_(added.nonEmpty)
                      (reset.toAsync *> fetch *> settle.toAsync).runAsync
        // Every summary present at request time has been replaced, so the request is done.
        _        <- useEffectWithDeps((request.get, rows.value)): (req, summaries) =>
                      import ctx.given
                      req
                        .filterNot(_.anyPending(summaries))
                        .fold(Callback.empty)(_ => (request.set(none).toAsync *> timeout.cancel).runAsync)
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
            req    <- IO(ProposalSummaries.Request(props.attachments.get))
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

        val tooltip =
          if (request.get.isDefined) "Generating..."
          else if (props.hasProposalErrors) "Fix proposal errors first"
          else "Regenerate the PDF summary"

        val title =
          Button(
            severity = Button.Severity.Secondary,
            icon = Icons.Gears,
            loading = request.get.isDefined,
            disabled = request.get.isDefined || props.hasProposalErrors,
            tooltip = tooltip,
            onClick = regenerate.runAsync
          ).tiny.compact.unless(props.readOnly)

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
