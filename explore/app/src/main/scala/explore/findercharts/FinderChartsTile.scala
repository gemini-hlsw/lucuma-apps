// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.findercharts

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.syntax.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.attachments.*
import explore.common.UserPreferencesQueries.FinderChartPreferences
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Attachment
import explore.model.AttachmentList
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.Transformation
import explore.model.enums.TileSizeState
import explore.model.reusability.given
import explore.utils.OdbRestClient
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.model.Program
import lucuma.core.util.Timestamp
import lucuma.ui.components.SolarProgress
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.pot.*

import scala.collection.immutable.SortedSet

final case class FinderChartsTile(
  programId:           Program.Id,
  oid:                 Observation.Id,
  attachmentIds:       View[SortedSet[Attachment.Id]],
  authToken:           Option[NonEmptyString],
  attachments:         View[AttachmentList],
  parallacticAngle:    Option[Angle],
  readonly:            Boolean,
  override val hidden: Boolean
) extends Tile[FinderChartsTile](
      ObsTabTileIds.FinderChartsId.id,
      "Finder Charts",
      bodyClass = ExploreStyles.FinderChartsTile
    )(FinderChartsTile)

object FinderChartsTile
    extends TileComponent[FinderChartsTile]({ (props, tileSize) =>
      import ObsAttachmentUtils.*
      import FinderChartsAttachmentUtils.*

      for
        ctx           <- useContext(AppContext.ctx)
        chartSelector <- useStateView(ChartSelector.Closed)
        selected      <- useStateView(none[Attachment.Id])
        added         <- useState(none[Attachment.Id])
        clientOpt     <- useMemo(props.authToken):
                           _.map: token =>
                             OdbRestClient[IO](ctx.odbRestURI, token)
        transform     <- useStateView(Transformation.Default) // Current transformation
        urlMap        <- useStateView[UrlMap](Map.empty)
        // added attachment, FIXME once we can upload and assign in one step
        added         <- useState(none[Attachment.Id])
        action        <-                                      // If added associate with the observation
          useEffectWithDeps(added.value): _ =>
            // Associate the newly added attachment with the observation and select it
            added.value
              .map: newlyAdded =>
                props.attachmentIds.mod(_ + newlyAdded) *>
                  transform.set(Transformation.Default) *>
                  selected.set(newlyAdded.some) *>
                  added.setState(none)
              .getOrEmpty
        _             <-
          useEffectWithDeps(
            (props.authToken, props.attachments.get, props.attachmentIds.get, clientOpt)
          ): (_, obsAttachments, obsAttachmentIds, clientOpt) =>
            clientOpt.value.foldMap: client =>
              val allCurrentKeys =
                validAttachments(obsAttachments, obsAttachmentIds).values.map(_.toMapKey).toSet

              val newOas: List[(Attachment.Id, Timestamp)] =
                allCurrentKeys.filter(key => !urlMap.get.contains(key)).toList

              val updateUrlMap: IO[Unit] =
                urlMap.mod { umap =>
                  val filteredMap = umap.filter((k, _) => allCurrentKeys.contains(k))
                  newOas.foldRight(filteredMap)((key, m) => m.updated(key, pending))
                }.toAsync

              val getUrls =
                newOas.traverse_(key => getAttachmentUrl(client, key, urlMap))

              val defaultSelected =
                if (allCurrentKeys.size === 1)
                  selected.set(allCurrentKeys.headOption.map(_._1))
                else Callback.empty

              updateUrlMap *> getUrls *> defaultSelected.to[IO]
        _             <-                                      // Read preferences
          useEffectWithDeps((props.oid, selected.get)): (oid, aid) =>
            import ctx.given

            aid
              .map: aid =>
                FinderChartPreferences
                  .queryWithDefault[IO](oid, aid)
                  .flatMap(transform.set(_).to[IO])
                  .runAsyncAndForget
              .getOrEmpty
        _             <-                                      // Write preferences
          useEffectWithDeps(transform.get): transform =>
            import ctx.given

            selected.get
              .map: aid =>
                FinderChartPreferences
                  .updateTransformation[IO](props.oid, aid, transform)
                  .runAsyncAndForget
              .getOrEmpty
        action        <- useStateView(Action.None)
      yield
        val title =
          if (tileSize.isMinimized) {
            // If minimized only indicate if there is a chart
            val count = props.attachmentIds.get.size
            val msg   = props.attachments.get.toList match {
              case Nil => "(0)"
              case _   => s"($count)"
            }
            <.div(ExploreStyles.FinderChartsTileTitle, msg)
          } else
            // If normal size show the selector
            attachmentSelector(
              props.programId,
              props.attachmentIds,
              props.attachments,
              props.authToken,
              selected,
              added,
              chartSelector,
              props.readonly
            )

        val transforms: List[String] = transform.get.calcTransform

        val body =
          <.div(
            ExploreStyles.FinderChartsBackground,
            ^.onClick ==> { _ =>
              chartSelector.set(ChartSelector.Closed).when_(chartSelector.get.value)
            }
          )(
            <.div(
              SolarProgress(css = ExploreStyles.FinderChartsLoadProgress)
                .unless(action.get === Action.None)
            ),
            ControlOverlay(props.parallacticAngle, transform),
            if (chartSelector.get.value)
              clientOpt.value.map: client =>
                FinderChartLinker(
                  props.programId,
                  client,
                  selected,
                  props.attachmentIds,
                  props.attachments.get
                )
            else EmptyVdom,
            <.div(ExploreStyles.FinderChartsBody)(
              selected.get.map { attId =>
                urlMap.get.find { case ((i, _), _) => i === attId }.map { url =>
                  url._2.renderPot(url =>
                    <.img(
                      ExploreStyles.FinderChartsImage,
                      ExploreStyles.FinderChartsImageInverted.when(transform.get.inverted.value),
                      ^.transform := transforms.mkString(" "),
                      ^.src       := url
                    )
                  )
                }
              }
            )
          )

        TileContents(title, body)
    })
