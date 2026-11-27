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
import lucuma.core.enums.ScorpioFilter
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.highcharts.Chart
import lucuma.typed.highcharts.mod.*
import lucuma.ui.format.DurationPaddedFormatter
import lucuma.ui.syntax.all.given

import java.time.Duration
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

/** Gantt style view of one step of a SCORPIO block: a row per channel on a shared time axis. */
final case class ScorpioBlockTimelineChart(timeline: ScorpioBlockTimeline, repeats: PosInt)
    extends ReactFnProps(ScorpioBlockTimelineChart.component)

object ScorpioBlockTimelineChart:
  private val ChartHeight: Double = 150
  private val BarWidth: Double    = 14

  private val Channels: List[ScorpioFilter] = ScorpioFilter.visible.toList

  private def millis(ts: TimeSpan): Double = ts.toMilliseconds.toDouble

  private def formatDuration(ts: TimeSpan): String = DurationPaddedFormatter(ts.toDuration)

  private def formatElapsed(ms: Double): String =
    DurationPaddedFormatter(Duration.ofMillis(ms.toLong))

  private def tooltip(segment: Segment): String =
    s"<strong>${segment.channel.shortName}</strong> · " +
      s" ${segment.exposureIndex + 1} / ${segment.exposureCount.value}<br/>" +
      s"${segment.kind.tag} <strong>${formatDuration(segment.duration)}</strong>"

  // Every step of a block has the same exposures, so the first one stands for all of them.
  private def chartOptions(step: Step): Options =
    val segments: List[Segment] =
      Channels.flatMap(channel => step.segments.filter(_.channel === channel))

    val tooltips: Vector[String] = segments.toVector.map(tooltip)

    val points: js.Array[XrangePointOptionsObject] =
      segments
        .map: s =>
          XrangePointOptionsObject()
            .setX(millis(s.start))
            .setX2(millis(s.end))
            .setY(Channels.indexOf(s.channel).toDouble)
            .setClassName(
              s.kind match
                case SegmentKind.Science => s"scorpio-block-${s.channel.shortName}"
                case kind                => s"scorpio-block-${kind.tag}"
            )
        .toJSArray

    val tooltipFormatter: TooltipFormatterCallbackFunction =
      (point: Point, _: Tooltip, _: js.UndefOr[Point]) => tooltips.lift(point.index.toInt).orEmpty

    val axisLabelFormatter: AxisLabelsFormatterCallbackFunction =
      (ctx: AxisLabelsFormatterContextObject, _: AxisLabelsFormatterContextObject) =>
        (ctx.value: Any) match
          case ms: Double => formatElapsed(ms)
          case other      => other.toString

    Options()
      .setChart(ChartOptions().setStyledMode(true).setHeight(ChartHeight))
      .setTitle(TitleOptions().setTextUndefined)
      .setCredits(CreditsOptions().setEnabled(false))
      .setLegend(LegendOptions().setEnabled(false))
      .setTooltip(TooltipOptions().setFormatter(tooltipFormatter))
      .setXAxis(
        List(
          XAxisOptions()
            .setType(AxisTypeValue.datetime)
            .setMin(0)
            .setMax(millis(step.duration))
            .setStartOnTick(false)
            .setEndOnTick(false)
            .setLabels(XAxisLabelsOptions().setFormatter(axisLabelFormatter))
        ).toJSArray
      )
      .setYAxis(
        List(
          YAxisOptions()
            .setTitle(YAxisTitleOptions().setText(""))
            .setCategories(Channels.map(_.shortName).toJSArray)
            .setReversed(true)
        ).toJSArray
      )
      .setPlotOptions(PlotOptions().setSeries(PlotSeriesOptions().setAnimation(false)))
      .setSeries(
        js.Array(
          SeriesXrangeOptions((), (), ())
            .setName("Step")
            .setPointWidth(BarWidth)
            .setBorderRadius(0)
            .setTurboThreshold(0)
            .setData(points)
            .asInstanceOf[SeriesOptionsType]
        )
      )

  private val component = ScalaFnComponent[ScorpioBlockTimelineChart]: props =>
    for options <- useMemo(props.timeline.steps.head)(chartOptions)
    yield
      val timeline: ScorpioBlockTimeline = props.timeline
      val total: TimeSpan                = timeline.duration *| props.repeats.value
      val stepCount: Int                 = timeline.steps.length

      def plural(n: Int, noun: String): String = if n === 1 then s"1 $noun" else s"$n ${noun}s"

      <.div(ExploreStyles.ScorpioBlockTimeline)(
        // Highcharts matches points by x when it updates a chart in place, and the rows share x
        // values, so a changed step gets a new chart instead.
        React.Fragment.withKey(timeline.steps.head.hashCode.toString)(
          Chart(options).withModules(Chart.Module.XRange)
        ),
        <.div(ExploreStyles.ScorpioBlockTimelineCaption)(
          <.span("Step ", <.strong(formatDuration(timeline.steps.head.duration))),
          <.span(
            s"Block of ${plural(stepCount, "step")} ",
            <.strong(formatDuration(timeline.duration))
          ),
          <.span(
            s"${plural(props.repeats.value, "repeat")} ",
            <.strong(formatDuration(total))
          ),
          <.span(ExploreStyles.ScorpioBlockTimelineReadout, "Readout"),
          <.span(ExploreStyles.ScorpioBlockTimelineWrite, "Write"),
          <.span(
            ExploreStyles.ScorpioBlockTimelineNote,
            "Science steps only, telescope offsets not included"
          )
        )
      )
