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
import lucuma.itc.SeriesDataType
import lucuma.itc.client.GraphResult
import lucuma.itc.client.SeriesResult
import lucuma.itc.math.roundToSignificantFigures
import lucuma.react.common.ReactFnProps
import lucuma.react.highcharts.Chart
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

  // Height of one stacked wavelength axis: its title and labels plus a couple
  // of pixels of breathing room, so the two stack without colliding.
  private val WavelengthAxisHeight = 40

  // Pulls the labels down against their axis line, closing the gap Highcharts
  // still reserves for the ticks we turned off.
  private val WavelengthLabelDrop = 8

  // The signal series carry the actual spectra; the background ones share
  // their slit's wavelength scale and would only duplicate an axis.
  private def isSignalSeries(seriesType: SeriesDataType): Boolean = seriesType match
    case SeriesDataType.SignalData | SeriesDataType.PixSigData => true
    case _                                                     => false

  private def chartOptions(
    graph:            GraphResult,
    seriesPerCcd:     Int,
    ccdRanges:        List[(Double, Double)],
    ccdLabels:        Map[NonNegInt, String],
    targetName:       String,
    signalToNoiseAt:  Wavelength,
    wavelengthRanges: Map[String, (Double, Double)]
  ) = {
    val yAxis            = graph.series.foldLeft(YAxis.Empty)(_ ∪ _.yAxis.yAxis)
    val title            = graph.graphType match
      case GraphType.SignalGraph | GraphType.SignalPixelGraph =>
        "𝐞⁻ per exposure per spectral pixel"
      case GraphType.S2NGraph                                 => "S/N per spectral pixel"
    val (min, max, tick) = yAxis.ticks(10)

    // The pixel-signal graph is plotted against detector pixel rather than
    // wavelength, and the GMOS IFU-2 spectra run right to left on the detector,
    // so the axis decreases, matching the web ITC.
    val isPixelGraph = graph.graphType === GraphType.SignalPixelGraph
    val xAxisTitle   = if isPixelGraph then "Pixels" else "Wavelength (nm)"
    val xAxisUnit    = if isPixelGraph then "px" else "nm"

    // GMOS returns one block of series per CCD, each block repeating the same
    // kinds with the CCD name appended:
    //   Blue Slit Signal, ..., Blue Slit Signal HSC, ..., Blue Slit Signal BB(R)
    // Detect the block length by looking for the first repeat of the leading
    // title, so we only key off the blocks when the series really have that
    // shape.
    val blockSize: Int =
      graph.series.headOption
        .map(head => graph.series.indexWhere(_.title.startsWith(s"${head.title} "), 1))
        .filter(_ > 0)
        .getOrElse(graph.series.length)

    val blocks: List[List[SeriesResult]] =
      if blockSize > 0 then graph.series.grouped(blockSize).toList else Nil

    val hasCcdBlocks: Boolean =
      isPixelGraph && blocks.sizeIs > 1 && blocks.forall: block =>
        block.sizeIs == blockSize &&
          block.zip(blocks.head).forall((s, first) => s.title.startsWith(first.title))

    // Cycle the palette over a block instead of over the whole series list, so
    // a kind keeps its color on every CCD, the way the web ITC plots it. It
    // also keeps every color index inside the range the stylesheet defines.
    val colorCycle: Int = if hasCcdBlocks then blockSize else seriesPerCcd

    val xBounds: Option[(Double, Double)] =
      Option.when(graph.series.nonEmpty):
        val edges = graph.series.flatMap(s => List(s.xAxis.start, s.xAxis.end))
        (edges.min, edges.max)

    // The two IFU-2 slits land on the same pixels but at different wavelengths.
    // The signal graph holds the very same series in the wavelength domain,
    // which gives us each slit's (linear) pixel-to-wavelength relation; the
    // first CCD block is enough, as the relation holds across the whole
    // detector.
    val wavelengthScales: List[(String, Double => Double)] =
      if hasCcdBlocks then
        for
          series         <- blocks.headOption.toList.flatten
          if isSignalSeries(series.seriesType)
          (wStart, wEnd) <- wavelengthRanges.get(series.title.trim).toList
          pStart          = series.xAxis.start
          pEnd            = series.xAxis.end
          if pStart =!= pEnd
          nmPerPixel      = (wEnd - wStart) / (pEnd - pStart)
        yield (series.title.trim.stripSuffix(" Signal"),
               (pixel: Double) => wStart + (pixel - pStart) * nmPerPixel
        )
      else Nil

    // One wavelength axis per slit above the plot, as the web ITC has.
    val wavelengthAxes: List[(String, (Double, Double))] =
      for
        (xMin, xMax)         <- xBounds.toList
        (slit, toWavelength) <- wavelengthScales
      yield
        val atMin = toWavelength(xMin)
        val atMax = toWavelength(xMax)
        (slit, (atMin.min(atMax), atMin.max(atMax)))

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
      (point: Point, _: Tooltip, _: js.UndefOr[Point]) =>
        val x: String               = rounded(point.x)
        val y: String               = rounded(point.y)
        val measUnit: String        =
          if (graph.graphType === GraphType.S2NGraph) "" else " 𝐞⁻"
        val classNames: String      =
          graphClassName + point.colorIndex.toOption.foldMap(ci => s" highcharts-color-${ci.toInt}")
        // A single pixel means a different wavelength in each slit, so spell
        // both of them out next to it. Empty for the wavelength-domain graphs.
        val slitWavelengths: String =
          wavelengthScales
            .map((slit, toWavelength) => f" | $slit ${toWavelength(point.x)}%.1f nm")
            .mkString
        s"""<strong>$x $xAxisUnit$slitWavelengths</strong><br/><span class="$classNames">●</span> ${point.series.name}: <strong>$y$measUnit</strong>"""

    val graphTitle = graph.graphType match
      case GraphType.SignalGraph      => "Signal in 1-pixel"
      case GraphType.S2NGraph         => "Signal / Noise"
      case GraphType.SignalPixelGraph => "IFU-2 Pixel Signal "

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

    val primaryXAxis =
      val base = XAxisOptions()
        .setType(AxisTypeValue.linear)
        .setTitle(XAxisTitleOptions().setText(xAxisTitle))
        .setReversed(isPixelGraph)
        .setPlotLines(plotLines)
        .setPlotBands(plotBands)
      // Pin the extremes, otherwise Highcharts pads each axis on its own and
      // the wavelength scales above no longer line up with the pixels below.
      if wavelengthAxes.isEmpty then base
      else
        xBounds.fold(base): (xMin, xMax) =>
          base.setMin(xMin).setMax(xMax).setStartOnTick(false).setEndOnTick(false)

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
        // Reversed: Highcharts stacks the first opposite axis closest to the
        // plot, and the web ITC shows the blue slit as the outermost one.
        (primaryXAxis :: wavelengthAxes.reverse.zipWithIndex.map: (axis, idx) =>
          val (slit, bounds) = axis
          XAxisOptions()
            .setType(AxisTypeValue.linear)
            .setOpposite(true)
            .setTitle(XAxisTitleOptions().setText(s"Wavelength (nm) ($slit)").setMargin(0))
            .setMin(bounds._1)
            .setMax(bounds._2)
            .setStartOnTick(false)
            .setEndOnTick(false)
            .setGridLineWidth(0)
            .setLineWidth(1)
            // These are reference scales stacked above the plot, so they get
            // stub ticks and should cost as little height as possible. On a top
            // axis Highcharts ignores labels.distance and places labels a fixed
            // drop above the line regardless of tick length; labels.y pulls
            // them back down against it.
            .setLabels(XAxisLabelsOptions().setY(WavelengthLabelDrop))
            .setTickWidth(1)
            .setTickLength(2)
            // Stack them by hand: left to itself Highcharts reserves far more
            // room per axis than a title-plus-labels strip actually needs.
            .setOffset(idx * WavelengthAxisHeight)
            .setShowEmpty(true)
        ).toJSArray
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
            val colorIdx                                        = if (colorCycle > 0) idx % colorCycle else idx
            val ccdIdx                                          = idx / seriesPerCcd
            def mkName(seriesName: String, ccdIdx: Int): String =
              ccdLabels
                .get(NonNegInt.unsafeFrom(ccdIdx))
                .map(label => s"$label $seriesName")
                .getOrElse(seriesName)
            val id                                              = s"series-$idx"
            SeriesLineOptions((), ())
              .setName(mkName(series.title, ccdIdx))
              .setYAxis(0)
              .setData(
                // The pixel graph comes back with x descending; Highcharts
                // needs it the other way around.
                series.data
                  .sortBy(_._1)
                  .map(p => (p(0), p(1)): Chart.Data)
                  .toJSArray
              )
              .setClassName(graphClassName)
              .setLineWidth(1)
              .setColorIndex(colorIdx.toDouble)
              .setLabel(SeriesLabelOptionsObject().setEnabled(false))
              .setId(id)
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
            // The pixel graph is plotted against detector pixel, but the signal
            // graph carries the same series against wavelength, which is where
            // the per-slit wavelength axes come from.
            val wavelengthRanges: Map[String, (Double, Double)] =
              graphs.toList
                .filter(_.graphType === GraphType.SignalGraph)
                .flatMap(_.series)
                .map(s => s.title.trim -> (s.xAxis.start, s.xAxis.end))
                .toMap

            // Some instruments like igrins2 returne a chart per ccd
            graphs.toList
              .groupBy(_.graphType)
              .map: (graphType, groupedGraphs) =>
                val seriesPerCcd =
                  groupedGraphs.headOption.foldMap(_.series.length)

                val ccdRanges = groupedGraphs.flatMap: gr =>
                  gr.series.headOption.map: s =>
                    (s.xAxis.start, s.xAxis.end)

                val merged =
                  GraphResult(graphType, groupedGraphs.flatMap(_.series))
                graphType ->
                  chartOptions(merged,
                               seriesPerCcd,
                               ccdRanges,
                               ccdLabels,
                               targetName,
                               signalToNoiseAt,
                               wavelengthRanges
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
