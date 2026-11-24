// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import explore.components.ui.ExploreStyles
import explore.model.ScorpioBlockTimeline
import explore.model.ScorpioBlockTimeline.*
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Offset
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.highcharts.Chart
import lucuma.typed.highcharts.mod.*
import lucuma.ui.syntax.all.given

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

/** Gantt style view of one SCORPIO block: a row per channel on a shared elapsed time axis. */
final case class ScorpioBlockTimelineChart(timeline: ScorpioBlockTimeline, repeats: PosInt)
    extends ReactFnProps(ScorpioBlockTimelineChart.component)

object ScorpioBlockTimelineChart:
  private val ChartHeight: Double = 150
  private val BarWidth: Double    = 14

  private val Channels: List[Channel] = Enumerated[Channel].all

  private def millis(ts: TimeSpan): Double = ts.toMilliseconds.toDouble

  private def formatDuration(ts: TimeSpan): String =
    val total: Long = ts.toSeconds.setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong
    val (h, m, s)   = (total / 3600, (total % 3600) / 60, total % 60)
    if h > 0 then f"$h%d:$m%02d:$s%02d"
    else if m > 0 then f"$m%d:$s%02d"
    else s"$s s"

  private def formatOffset(offset: Offset): String =
    val (p, q) = Offset.signedDecimalArcseconds.get(offset)
    f"p $p%.2f″, q $q%.2f″"

  private def tooltip(step: Step, segment: Segment): String =
    s"<strong>${segment.channel.tag}</strong> · step ${step.label} (${formatOffset(step.telescopeConfig.offset)})<br/>" +
      s"Exposure ${segment.exposureIndex + 1} of ${segment.exposureCount.value} · " +
      s"${segment.kind.tag} <strong>${formatDuration(segment.duration)}</strong>"

  private def chartOptions(timeline: ScorpioBlockTimeline): Options =
    val steps: List[Step] = timeline.steps.toList

    val segments: List[Segment] = Channels.flatMap(timeline.segments)

    val tooltips: Vector[String] =
      segments.toVector.flatMap(s => steps.lift(s.stepIndex).map(tooltip(_, s)))

    val points: js.Array[XrangePointOptionsObject] =
      segments
        .map: s =>
          XrangePointOptionsObject()
            .setX(millis(s.start))
            .setX2(millis(s.end))
            .setY(Channels.indexOf(s.channel).toDouble)
            .setClassName(
              s.kind match
                case SegmentKind.Exposure => s"scorpio-block-${s.channel.tag}"
                case kind                 => s"scorpio-block-${kind.tag}"
            )
        .toJSArray

    val tooltipFormatter: TooltipFormatterCallbackFunction =
      (point: Point, _: Tooltip, _: js.UndefOr[Point]) => tooltips.lift(point.index.toInt).orEmpty

    val stepBands: js.Array[XAxisPlotBandsOptions] =
      steps
        .map: step =>
          XAxisPlotBandsOptions()
            .setFrom(millis(step.start))
            .setTo(millis(step.end))
            .setClassName(if step.index % 2 === 1 then "scorpio-block-step-alt" else "")
            .setLabel(
              XAxisPlotBandsLabelOptions()
                .setText(step.label)
                .setAlign(AlignValue.center)
                .setVerticalAlign(VerticalAlignValue.top)
                .setY(-4)
            )
        .toJSArray

    val stepLines: js.Array[XAxisPlotLinesOptions] =
      steps
        .drop(1)
        .map(step => XAxisPlotLinesOptions().setValue(millis(step.start)).setWidth(1).setZIndex(3))
        .toJSArray

    Options()
      .setChart(ChartOptions().setStyledMode(true).setHeight(ChartHeight).setMarginTop(22))
      .setTitle(TitleOptions().setTextUndefined)
      .setCredits(CreditsOptions().setEnabled(false))
      .setLegend(LegendOptions().setEnabled(false))
      .setTooltip(TooltipOptions().setFormatter(tooltipFormatter))
      .setXAxis(
        List(
          XAxisOptions()
            .setType(AxisTypeValue.datetime)
            .setMin(0)
            .setMax(millis(timeline.duration))
            .setStartOnTick(false)
            .setEndOnTick(false)
            .setPlotBands(stepBands)
            .setPlotLines(stepLines)
        ).toJSArray
      )
      .setYAxis(
        List(
          YAxisOptions()
            .setTitle(YAxisTitleOptions().setText(""))
            .setCategories(Channels.map(_.tag).toJSArray)
            .setReversed(true)
        ).toJSArray
      )
      .setSeries(
        js.Array(
          SeriesXrangeOptions((), (), ())
            .setName("Block")
            .setPointWidth(BarWidth)
            .setBorderRadius(0)
            .setTurboThreshold(0)
            .setData(points)
            .asInstanceOf[SeriesOptionsType]
        )
      )

  private val component = ScalaFnComponent[ScorpioBlockTimelineChart]: props =>
    for options <- useMemo(props.timeline)(chartOptions)
    yield
      val total: TimeSpan = props.timeline.duration *| props.repeats.value
      val repeats: String =
        if props.repeats.value === 1 then "1 repeat" else s"${props.repeats.value} repeats"

      <.div(ExploreStyles.ScorpioBlockTimeline)(
        Chart(options).withModules(Chart.Module.XRange),
        <.div(ExploreStyles.ScorpioBlockTimelineCaption)(
          <.span("Block ", <.strong(formatDuration(props.timeline.duration))),
          <.span(s"$repeats ", <.strong(formatDuration(total))),
          <.span(ExploreStyles.ScorpioBlockTimelineReadout, "Readout"),
          <.span(ExploreStyles.ScorpioBlockTimelineWrite, "Write"),
          <.span(
            ExploreStyles.ScorpioBlockTimelineNote,
            "Science steps only, telescope offsets not included"
          )
        )
      )
