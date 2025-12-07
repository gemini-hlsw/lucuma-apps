// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.plots

import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.highcharts.Chart

import scalajs.js

enum SeriesType(
  val name:      String,
  val cssClass:  String,
  val yAxis:     Int,
  val threshold: Int,
  val data:      ObjectPlotData.SeriesData => js.Array[Chart.Data]
) derives Enumerated:
  val tag: String = name

  case Elevation
      extends SeriesType(
        "Elevation",
        "elevation",
        0,
        90,
        _.targetAltitude.asInstanceOf[js.Array[Chart.Data]]
      )
  case ParallacticAngle
      extends SeriesType(
        "Parallactic Angle",
        "parallactic-angle",
        1,
        -180,
        _.parallacticAngle.asInstanceOf[js.Array[Chart.Data]]
      )
  case SkyBrightness
      extends SeriesType(
        "Sky Brightness",
        "sky-brightness",
        2,
        22,
        _.skyBrightness.asInstanceOf[js.Array[Chart.Data]]
      )
  case LunarElevation
      extends SeriesType(
        "Lunar Elevation",
        "lunar-elevation",
        0,
        90,
        _.moonAltitude.asInstanceOf[js.Array[Chart.Data]]
      )

object SeriesType:
  given Display[SeriesType] = Display.byShortName(_.name)

  def fromSeriesIndex(seriesIndex: Int): SeriesType =
    fromOrdinal:
      if seriesIndex < 4 then seriesIndex
      else (seriesIndex - 4) % 3
