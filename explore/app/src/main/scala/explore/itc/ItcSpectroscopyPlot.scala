// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyChain
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import explore.components.ui.ExploreStyles
import explore.highcharts.*
import explore.model.itc.*
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Wavelength
import lucuma.itc.GraphType
import lucuma.itc.ItcCcd
import lucuma.itc.client.GraphResult
import lucuma.itc.math.roundToSignificantFigures
import lucuma.react.common.ReactFnProps
import lucuma.react.highcharts.Chart
import lucuma.typed.highcharts.highchartsStrings.line
import lucuma.typed.highcharts.mod.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

case class ItcSpectroscopyPlot(
  ccds:            NonEmptyChain[ItcCcd],
  graphs:          NonEmptyChain[GraphResult],
  graphType:       GraphType,
  targetName:      String,
  signalToNoiseAt: Wavelength,
  details:         PlotDetails,
  ccdLabels:       Map[NonNegInt, String]
) extends ReactFnProps(ItcSpectroscopyPlot.component)

object ItcSpectroscopyPlot {
  private given Reusability[Map[NonNegInt, String]] = Reusability.map

  private def chartOptions(
    graph:           GraphResult,
    seriesPerCcd:    Int,
    ccdRanges:       List[(Double, Double)],
    ccdLabels:       Map[NonNegInt, String],
    targetName:      String,
    signalToNoiseAt: Wavelength
  ) = {
    val yAxis            = graph.series.foldLeft(YAxis.Empty)(_ ∪ _.yAxis.yAxis)
    val title            = graph.graphType match
      case GraphType.SignalGraph      => "𝐞⁻ per exposure per spectral pixel"
      case GraphType.S2NGraph         => "S/N per spectral pixel"
      case GraphType.SignalPixelGraph => "S/N per pixel"
    val (min, max, tick) = yAxis.ticks(10)

    val yAxes = YAxisOptions()
      .setTitle(YAxisTitleOptions().setText(title))
      .setAllowDecimals(false)
      .setTickInterval(tick)
      .setMin(min)
      .setMax(max)
      .setFloor(0.0) // Y can never be negative
      .setMinorTickInterval(tick / 3)
      .setLineWidth(1)
      .setLabels(YAxisLabelsOptions().setFormat("{value}"))

    val graphClassName = graph.graphType.toString.toLowerCase()

    def rounded(x: js.UndefOr[Double | String]): String =
      x.toOption.fold("-") {
        case x: Double => roundToSignificantFigures(x, 4).toString
        case x: String => x
      }

    val tooltipFormatter: TooltipFormatterCallbackFunction =
      (point: Point, _: Tooltip) =>
        val x: String          = rounded(point.x)
        val y: String          = rounded(point.y)
        val measUnit: String   = if (graph.graphType === GraphType.SignalGraph) " 𝐞⁻" else ""
        val classNames: String =
          graphClassName + point.colorIndex.toOption.foldMap(ci => s" highcharts-color-${ci.toInt}")
        s"""<strong>$x nm</strong><br/><span class="$classNames">●</span> ${point.series.name}: <strong>$y$measUnit</strong>"""

    val graphTitle = graph.graphType match
      case GraphType.SignalGraph      => "Signal in 1-pixel"
      case GraphType.S2NGraph         => "Signal / Noise"
      case GraphType.SignalPixelGraph => "Pixel"

    val plotLines = graph.graphType match
      case GraphType.SignalGraph | GraphType.SignalPixelGraph => js.Array()
      case GraphType.S2NGraph                                 =>
        val value = signalToNoiseAt.toNanometers.value.value.toDouble
        List(
          XAxisPlotLinesOptions()
            .setDashStyle(DashStyleValue.LongDash)
            .setWidth(3)
            .setValue(value)
            .clazz(ExploreStyles.ItcPlotWvPlotLine)
            .setZIndex(10)
            .setLabel(XAxisPlotLinesLabelOptions().setText(f"$value%.1f nm"))
        ).toJSArray

    val hasCcdLabels =
      ccdRanges.length > 1 && ccdRanges.indices.forall(i => ccdLabels.exists(_._1.value === i))

    val plotBands =
      if (ccdRanges.length > 1)
        ccdRanges.zipWithIndex
          .map: (range, idx) =>
            // from zipWithIndex we know it starts at 0
            val index = NonNegInt.unsafeFrom(idx)

            val band = XAxisPlotBandsOptions()
              .setFrom(range._1)
              .setTo(range._2)
              .setClassName(s"plot-band-ccd-$idx")

            if hasCcdLabels then
              band.setLabel(
                XAxisPlotBandsLabelOptions()
                  .setText(ccdLabels(index))
                  .setAlign(AlignValue.center)
                  .setVerticalAlign(VerticalAlignValue.top)
                  .setY(-15)
              )
            else band
          .toJSArray
      else js.Array()

    Options()
      .setChart:
        CommonOptions.clazz(ExploreStyles.ItcPlotChart)
      .setTitle(TitleOptions().setText(graphTitle))
      .setSubtitle:
        SubtitleOptions().setText(targetName)
      .setCredits(CreditsOptions().setEnabled(false))
      .setLegend(LegendOptions().setMargin(0))
      .setTooltip(TooltipOptions().setFormatter(tooltipFormatter).setClassName(graphClassName))
      .setXAxis:
        XAxisOptions()
          .setType(AxisTypeValue.linear)
          .setTitle(XAxisTitleOptions().setText("Wavelength (nm)"))
          .setPlotLines(plotLines)
          .setPlotBands(plotBands)
      .setYAxis(List(yAxes).toJSArray)
      .setPlotOptions:
        PlotOptions()
          .setSeries(
            PlotSeriesOptions()
              .setLineWidth(4)
              .setMarker(PointMarkerOptionsObject().setEnabled(false).setRadius(0))
              .setStates(
                SeriesStatesOptionsObject()
                  .setHover(SeriesStatesHoverOptionsObject().setEnabled(false))
              )
          )
      .setSeries:
        graph.series.zipWithIndex
          .map: (series, idx) =>
            val colorIdx = if (seriesPerCcd > 0) idx % seriesPerCcd else idx
            val firstCcd = idx < seriesPerCcd
            val id       = s"series-$colorIdx"
            val opts     = SeriesLineOptions((), (), line)
              .setName(series.title)
              .setYAxis(0)
              .setData(
                series.data
                  .map(p => (p(0), p(1)): Chart.Data)
                  .toJSArray
              )
              .setClassName(graphClassName)
              .setLineWidth(1)
              .setColorIndex(colorIdx.toDouble)
              .setLabel(SeriesLabelOptionsObject().setEnabled(false))

            if (firstCcd) opts.setId(id)
            else
              opts
                .setLinkedTo(id)
                .setShowInLegend(false)
          .map(_.asInstanceOf[SeriesOptionsType])
          .toJSArray
  }

  private val EmptyGraphOptions: Reusable[Options] =
    Reusable.always {
      val yAxis = YAxisOptions()
        .setAllowDecimals(false)
        .setMin(0)
        .setMax(100)
        .setTickInterval(10)

      Options()
        .setChart(
          ChartOptions()
            .setStyledMode(true)
            .setAlignTicks(false)
            .clazz(ExploreStyles.ItcPlotChart)
            // Will be used in the future to persist the zoom
            // .selectionCB(s => Callback.log(s"selection ${s.xAxis(0).min}"))
        )
        .setTitle(TitleOptions().setTextUndefined)
        .setCredits(CreditsOptions().setEnabled(false))
        .setXAxis(
          XAxisOptions()
            .setType(AxisTypeValue.linear)
        )
        .setYAxis(List(yAxis).toJSArray)
        .setPlotOptions(
          PlotOptions()
            .setSeries(
              PlotSeriesOptions()
                .setLineWidth(4)
                .setMarker(PointMarkerOptionsObject().setEnabled(false).setRadius(0))
                .setStates(
                  SeriesStatesOptionsObject()
                    .setHover(SeriesStatesHoverOptionsObject().setEnabled(false))
                )
            )
        )
    }

  private val component = ScalaFnComponent[ItcSpectroscopyPlot]: props =>
    for {
      itcGraphOptions <-
        useMemo((props.graphs, props.targetName, props.signalToNoiseAt, props.ccdLabels)):
          (graphs, targetName, signalToNoiseAt, ccdLabels) =>
            // Some instruments like igrins2 returne a chart per ccd
            graphs.toList
              .groupBy(_.graphType)
              .map: (graphType, groupedGraphs) =>
                val seriesPerCcd =
                  groupedGraphs.headOption.foldMap(_.series.length)

                val ccdRanges = groupedGraphs.flatMap: gr =>
                  gr.series.headOption.flatMap: s =>
                    s.xAxis.map: axis =>
                      (axis.start, axis.end)

                val merged =
                  GraphResult(graphType, groupedGraphs.flatMap(_.series))
                graphType ->
                  chartOptions(merged,
                               seriesPerCcd,
                               ccdRanges,
                               ccdLabels,
                               targetName,
                               signalToNoiseAt
                  )
      options         <- useMemo((props.graphType, itcGraphOptions)): (graphType, itcGraphOptions) =>
                           itcGraphOptions.get(graphType)
    } yield
      val chartOptions: Reusable[Options] = options.sequenceOption.getOrElse(EmptyGraphOptions)

      Chart(
        chartOptions,
        allowUpdate = false,
        containerMod = TagMod(ExploreStyles.ItcPlotBody)
      )
}
