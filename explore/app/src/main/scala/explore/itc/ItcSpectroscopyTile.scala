// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.EitherNec
import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.*
import explore.common.UserPreferencesQueries.*
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.GlobalPreferences
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.TargetList
import explore.model.itc.*
import explore.model.reusability.given
import explore.modes.ItcInstrumentConfig
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.itc.GraphType
import lucuma.react.primereact.Dropdown
import lucuma.react.primereact.Message
import lucuma.react.primereact.SelectItem
import lucuma.ui.format.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given

extension (tuple: (ItcTarget, Either[ItcQueryProblem, ItcGraphResult]))
  def toTargetAndResults: TargetAndResults =
    TargetAndResults(tuple._1, tuple._2)

final case class ItcSpectroscopyTile(
  userIdOpt:           Option[User.Id],
  observation:         Observation,
  selectedConfig:      Option[List[ItcInstrumentConfig]],
  obsTargets:          TargetList,
  customSedTimestamps: List[Timestamp],
  globalPreferences:   View[GlobalPreferences]
) extends Tile[ItcSpectroscopyTile](
      ObsTabTileIds.ItcId.id,
      "ITC",
      bodyClass = ExploreStyles.ItcTileBody
    )(ItcSpectroscopyTile)

object ItcSpectroscopyTile
    extends TileComponent[ItcSpectroscopyTile]({ (props, _) =>
      given Reusability[TargetAndResults] = Reusability.byEq

      for {
        ctx         <- useContext(AppContext.ctx)
        tileState   <- useStateView(ItcTileState.Empty)
        graphQuerier =
          ItcGraphQuerier(
            props.observation,
            props.selectedConfig.getOrElse(List.empty),
            props.obsTargets,
            props.customSedTimestamps
          )
        _           <-
          useEffectWithDeps(graphQuerier): querier =>
            import ctx.given

            tileState
              .zoom(ItcTileState.asterismResults)
              .set(Pot.pending)
              .toAsync >>
              querier.requestGraphs
                .flatMap { t =>
                  tileState
                    .zoom(ItcTileState.asterismResults)
                    .set(t.ready)
                    .toAsync
                }
        _           <- // Reset the selected target if the brightest target changes
          useEffectWithDeps(
            tileState.get.graphsBrightestOrFirst
          ): itcBrightestOrFirst =>
            tileState.zoom(ItcTileState.selectedTarget).set(itcBrightestOrFirst)
        _           <- // if the targets change, make sure the selected target is still available
          useEffectWithDeps(
            tileState.get.graphsTargets
          ): targets =>
            val selected = tileState.zoom(ItcTileState.selectedTarget)
            if (selected.get.exists(targets.contains))
              Callback.empty
            else
              selected.set(tileState.get.graphsBrightestOrFirst)
      } yield
        import ctx.given

        def singleSN: ItcGraphResult => VdomNode =
          (r: ItcGraphResult) => <.span(r.singleSNRatio.value.format)

        def totalSN: ItcGraphResult => VdomNode =
          (r: ItcGraphResult) => <.span(r.finalSNRatio.value.format)

        def snSection(title: String, fn: ItcGraphResult => VdomNode) =
          tileState.get.selectedTarget
            .flatMap(_.result.toOption)
            .map: result =>
              React.Fragment(
                <.label(title),
                fn(result)
              )

        val options =
          tileState.get.targetResults
            .map(t => SelectItem(label = t.target.name.value, value = t))

        val title: VdomNode =
          // The only way this should be empty is if there are no targets in the results.
          tileState.get.selectedTarget.map: (gr: TargetAndResults) =>
            <.div(
              ExploreStyles.ItcTileTitle,
              <.label(s"Target:"),
              Dropdown(
                clazz = ExploreStyles.ItcTileTargetSelector,
                value = gr,
                onChange = (o: TargetAndResults) => tileState.mod(_.copy(selectedTarget = o.some)),
                options = options
              ).when(options.length > 1),
              <.span(
                tileState.get.selectedTarget
                  .map((tar: TargetAndResults) => tar.target.name.value)
                  .getOrElse("-")
              )
                .when(options.length === 1),
              snSection("S/N per exposure:", singleSN),
              snSection("S/N Total:", totalSN)
            )

        def buildBody(
          userId:          User.Id,
          signalToNoiseAt: Wavelength,
          graphResult:     ItcGraphResult
        ): VdomNode =
          val globalPreferences: View[GlobalPreferences] =
            props.globalPreferences.withOnMod: prefs =>
              ItcPlotPreferences
                .updatePlotPreferences[IO](userId, prefs.itcChartType, prefs.itcDetailsOpen)
                .runAsyncAndForget

          val graphTypeView: View[GraphType] =
            globalPreferences.zoom(GlobalPreferences.itcChartType)

          val detailsView: View[PlotDetails] =
            globalPreferences.zoom(GlobalPreferences.itcDetailsOpen)

          def bandValues(sp: SourceProfile)(band: Band): Option[BrightnessValues.ForBand] =
            SourceProfile.integratedBrightnesses
              .getOption(sp)
              .flatMap(_.get(band))
              .orElse:
                SourceProfile.surfaceBrightnesses
                  .getOption(sp)
                  .flatMap(_.get(band))
              .map: b =>
                BrightnessValues.ForBand(band, b.value, b.units)

          def emissionLineValues(
            sp: SourceProfile
          )(wavelength: Wavelength): Option[BrightnessValues.ForEmissionLine] =
            SourceProfile.integratedWavelengthLines
              .getOption(sp)
              .flatMap(_.get(wavelength))
              .orElse:
                SourceProfile.surfaceWavelengthLines
                  .getOption(sp)
                  .flatMap(_.get(wavelength))
              .map: e =>
                BrightnessValues.ForEmissionLine(
                  wavelength,
                  e.lineWidth.value,
                  e.lineFlux.value,
                  e.lineFlux.units
                )

          val sourceProfile: SourceProfile =
            graphResult.target.input.sourceProfile

          val selectedTargetBrightness: Option[BrightnessValues] =
            graphResult.integrationTime.bandOrLine
              .fold(bandValues(sourceProfile), emissionLineValues(sourceProfile))

          <.div(
            ExploreStyles.ItcPlotSection,
            ExploreStyles.ItcPlotDetailsHidden.unless(detailsView.get.value)
          )(
            ItcSpectroscopyPlotDescription(
              selectedTargetBrightness,
              graphResult.itcExposureTime,
              graphResult.graphCcds,
              graphResult.finalSNRatio,
              graphResult.singleSNRatio
            ),
            ItcSpectroscopyPlot(
              graphResult.graphCcds,
              graphResult.graphData,
              graphTypeView.get,
              graphResult.target.name.value,
              signalToNoiseAt,
              detailsView.get
            ),
            ItcPlotControl(graphTypeView, detailsView)
          )

        val resultPot: Pot[EitherNec[ItcTargetProblem, (Wavelength, ItcGraphResult)]] =
          tileState.get.asterismResults.map:
            _.flatMap(agr =>
              tileState.get.selectedTarget
                .toRightNec(ItcQueryProblem.GenericError(Constants.NoTargets).toTargetProblem)
                .flatMap(_.asTargetProblem)
                .map(gr => (agr.signalToNoiseAt, gr))
            )

        val body: VdomNode =
          props.userIdOpt
            .map: userId =>
              resultPot.renderPot(
                valueRender = _.fold(
                  es =>
                    Message(
                      text = es.toList
                        .map(_.format)
                        .mkString("Could not generate a graph:\n", "\n", ""),
                      severity = Message.Severity.Warning
                    ),
                  (signalToNoiseAt, graphResult) => buildBody(userId, signalToNoiseAt, graphResult)
                )
              )

        TileContents(title, body)
    })
