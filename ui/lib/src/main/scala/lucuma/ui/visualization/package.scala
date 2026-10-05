// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^.VdomAttr
import lucuma.ags.AgsParams
import lucuma.ags.GuideStarCandidate
import lucuma.ags.SingleProbeAgsParams
import lucuma.core.enums.AltairMode
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.SequenceType
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.util.NewBoolean
import lucuma.react.common.Css
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.aladin.Fov

import scala.math.*

val canvasWidth      = VdomAttr("width")
val canvasHeight     = VdomAttr("height")
val svgFontSize      = VdomAttr("font-size")
val textAnchor       = VdomAttr("text-anchor")
val patternUnits     = VdomAttr("patternUnits")
val patternTransform = VdomAttr("patternTransform")

// The values on the geometry are in microarcseconds
// They are fairly large and break is some browsers
// We apply a scaling factor uniformil
inline def scale: Double => Double = (v: Double) => rint(v / 1000)

inline def reverseScale: Double => Double = (v: Double) => rint(v * 1000)

extension (offset: Offset)
  def micros: (Double, Double) = {
    // Offset amount
    val offP =
      Offset.P.signedDecimalArcseconds.get(offset.p).toDouble * 1e6

    val offQ =
      Offset.Q.signedDecimalArcseconds.get(offset.q).toDouble * 1e6
    (offP, offQ)
  }

def calculateViewBox(
  x:            Double,
  y:            Double,
  w:            Double,
  h:            Double,
  fov:          Fov,
  screenOffset: Offset
): (Double, Double, Double, Double) = {
  // Shift factors on x/y, basically the percentage shifted on x/y
  val px           = abs(x / w) - 0.5
  val py           = abs(y / h) - 0.5
  // scaling factors on x/y
  val sx           = fov.x.toMicroarcseconds / w
  val sy           = fov.y.toMicroarcseconds / h
  // Offset amount
  val (offP, offQ) = screenOffset.micros

  val viewBoxX = scale(x + px * w) * sx + scale(offP)
  val viewBoxY = scale(y + py * h) * sy + scale(offQ)
  val viewBoxW = scale(w) * sx
  val viewBoxH = scale(h) * sy
  (viewBoxX, viewBoxY, viewBoxW, viewBoxH)
}

// Pixels around the canvas where overlay elements are still drawn
val ClipMargin: Double = 100.0

/**
 * Clip the segment (x1, y1)-(x2, y2) to the rectangle (minX, minY)-(maxX, maxY) (Liang–Barsky).
 * Returns None if no part of the segment is inside the rectangle.
 */
def clipSegment(
  x1:   Double,
  y1:   Double,
  x2:   Double,
  y2:   Double,
  minX: Double,
  minY: Double,
  maxX: Double,
  maxY: Double
): Option[(Double, Double, Double, Double)] =
  val dx = x2 - x1
  val dy = y2 - y1

  // Each edge as (p, q): the segment is inside that edge where t * p <= q
  val edges = List((-dx, x1 - minX), (dx, maxX - x1), (-dy, y1 - minY), (dy, maxY - y1))

  edges
    .foldLeft(Option((0.0, 1.0))):
      case (Some((t0, t1)), (p, q)) =>
        if p == 0 then Option.when(q >= 0)((t0, t1))
        else
          val t        = q / p
          val (n0, n1) = if p < 0 then (t0.max(t), t1) else (t0, t1.min(t))
          Option.when(n0 <= n1)((n0, n1))
      case (None, _)                => None
    .map: (t0, t1) =>
      (x1 + t0 * dx, y1 + t0 * dy, x1 + t1 * dx, y1 + t1 * dy)

object TooltipState extends NewBoolean { inline def Open = True; inline def Closed = False }
type TooltipState = TooltipState.Type

/**
 * Path for a tooltip located above the point
 * https://medium.com/welldone-software/tooltips-using-svg-path-1bd69cc7becd
 */
def topTooltipPath(width: Double, height: Double, offset: Double, radius: Double): String =
  val left   = -width / 2
  val right  = width / 2
  val top    = -offset - height
  val bottom = -offset

  s"""M 0,0
    L ${-offset},${bottom}
    H ${left + radius}
    Q ${left},${bottom} ${left},${bottom - radius}
    V ${top + radius}
    Q ${left},${top} ${left + radius},${top}
    H ${right - radius}
    Q ${right},${top} ${right},${top + radius}
    V ${bottom - radius}
    Q ${right},${bottom} ${right - radius},${bottom}
    H ${offset}
    L 0,0 z""".stripMargin

/**
 * Path for a tooltip located below the point
 */
def bottomTooltipPath(width: Double, height: Double, offset: Double, radius: Double): String =
  val left   = -width / 2
  val right  = width / 2
  val bottom = offset + height
  val top    = offset
  s"""M 0,0
    L ${-offset},${top}
    H ${left + radius}
    Q ${left},${top} ${left},${top + radius}
    V ${bottom - radius}
    Q ${left},${bottom} ${left + radius},${bottom}
    H ${right - radius}
    Q ${right},${bottom} ${right},${bottom - radius}
    V ${top + radius}
    Q ${right},${top} ${right - radius},${top}
    H ${offset}
    L 0,0 z""".stripMargin

def textDomSize(textValue: String): (Double, Double) =
  val document = org.scalajs.dom.document
  val text     = document.createElement("span")
  document.body.appendChild(text)

  text.innerHTML = textValue;

  val width  = Math.ceil(text.clientWidth)
  val height = Math.ceil(text.clientHeight)

  document.body.removeChild(text)
  (width, height)

extension (target: GuideStarCandidate)
  protected def selector: Css =
    Css(s"guide-star-${target.id}")

def offsetIndicators(
  offsets:         Option[NonEmptyList[Offset]],
  baseCoordinates: Coordinates,
  posAngle:        Angle,
  oType:           SequenceType,
  css:             Css,
  visible:         Boolean
) =
  offsets
    .foldMap(_.toList)
    .zipWithIndex
    .map: (o, i) =>
      for
        idx <- refineV[NonNegative](i).toOption
        c   <- baseCoordinates.offsetBy(posAngle, o) if visible
      yield SvgTarget.OffsetIndicator(c, idx, o, oType, css, 4)

private val hatchTile = 15000 // mas (15 arcsec)

// Define a hatch pattern to fill certain svg shapes with diagonal lines.
def hatchPattern(id: String, colorClass: Css, angleDeg: Int, lineClass: Css): VdomNode =
  // import the locally to avoid collisions with html
  import japgolly.scalajs.react.vdom.svg_<^.*
  <.pattern(
    ^.id             := id,
    patternUnits     := "userSpaceOnUse",
    ^.width          := hatchTile,
    ^.height         := hatchTile,
    patternTransform := s"rotate($angleDeg)",
    <.rect(
      colorClass,
      ^.width       := hatchTile,
      ^.height      := hatchTile,
      ^.fillOpacity := "0.08"
    ),
    <.line(
      colorClass |+| lineClass,
      ^.x1          := "0",
      ^.y1          := "0",
      ^.x2          := "0",
      ^.y2          := hatchTile
    )
  )

def hatchDefs(hatchLine: Css, hatchLineSel: Css): VdomNode =
  // import the locally to avoid collisions with html
  import japgolly.scalajs.react.vdom.svg_<^.*
  <.defs(
    hatchPattern("ghost-ifu1-hatch", Css("ghost-ifu1-hatch-color"), 45, hatchLine),
    hatchPattern("ghost-ifu2-hatch", Css("ghost-ifu2-hatch-color"), -45, hatchLine),
    hatchPattern("ghost-ifu1-hatch-selected", Css("ghost-ifu1-hatch-color"), 45, hatchLineSel),
    hatchPattern("ghost-ifu2-hatch-selected", Css("ghost-ifu2-hatch-color"), -45, hatchLineSel)
  )

extension (conf: BasicConfiguration)
  /**
   * Labels drawn next to a geometry, keyed by the css the geometry is registered under. Only for
   * shapes a user cannot identify from position alone: the GMOS IFU sky field sits ~60" off the
   * base, so without a label it reads as a second science field.
   */
  def shapeLabels: List[(Css, String)] =
    conf match
      case BasicConfiguration.GmosNorthIfu(fpu = _) | BasicConfiguration.GmosSouthIfu(fpu = _) =>
        List(VisualizationStyles.GmosIfuSkyFov -> "Sky")
      case _                                                                                   =>
        List.empty

  def agsParams(
    port:       PortDisposition,
    guideProbe: Option[GuideProbe],
    altair:     Option[AltairMode]
  ): Option[AgsParams & SingleProbeAgsParams] =
    val base =
      conf match
        case BasicConfiguration.GmosNorthLongSlit(fpu = fpu)                                 =>
          AgsParams.GmosLongSlit(fpu.asLeft, port).some
        case BasicConfiguration.GmosSouthLongSlit(fpu = fpu)                                 =>
          AgsParams.GmosLongSlit(fpu.asRight, port).some
        case BasicConfiguration.GmosNorthMos(_, _, _, _)                                     =>
          AgsParams.GmosMos(Site.GN, port).some
        case BasicConfiguration.GmosSouthMos(_, _, _, _)                                     =>
          AgsParams.GmosMos(Site.GS, port).some
        case BasicConfiguration.GmosNorthIfu(fpu = fpu)                                      =>
          AgsParams.GmosIfu(fpu.asLeft, port).some
        case BasicConfiguration.GmosSouthIfu(fpu = fpu)                                      =>
          AgsParams.GmosIfu(fpu.asRight, port).some
        case BasicConfiguration.GmosNorthImaging(_)                                          =>
          AgsParams.GmosImaging(port).some
        case BasicConfiguration.GmosSouthImaging(_)                                          =>
          AgsParams.GmosImaging(port).some
        case BasicConfiguration.Flamingos2LongSlit(fpu = fpu)                                =>
          AgsParams
            .Flamingos2LongSlit(Flamingos2LyotWheel.F16, Flamingos2FpuMask.Builtin(fpu), port)
            .some
        case BasicConfiguration.Flamingos2Mos(_, _, _)                                       =>
          AgsParams.Flamingos2Mos(Flamingos2LyotWheel.F16, port).some
        case BasicConfiguration.Flamingos2Imaging(_)                                         =>
          AgsParams.Flamingos2Imaging(Flamingos2LyotWheel.F16, port).some
        case BasicConfiguration.Igrins2LongSlit                                              =>
          AgsParams.Igrins2LongSlit().some
        case BasicConfiguration.GnirsImaging(filters = filters, camera = camera)             =>
          AgsParams
            .GnirsImaging(camera, AgsParams.GnirsImaging.representativeFilter(filters), port)
            .guidedBy(guideProbe, altair)
            .some
        case BasicConfiguration.GnirsSpectroscopy(fpu = GnirsFpu.Spectroscopy.Ifu(ifu))      =>
          AgsParams.GnirsIfu(ifu, port).guidedBy(guideProbe, altair).some
        case BasicConfiguration.GnirsSpectroscopy(fpu = fpu, prism = prism, camera = camera) =>
          // Slit (or, defensively, any non-IFU fpu) → long-slit probe params.
          val slit = GnirsFpu.Spectroscopy.slit.getOption(fpu).getOrElse(GnirsFpuSlit.LongSlit_1_00)
          AgsParams.GnirsLongSlit(slit, camera, prism, port).guidedBy(guideProbe, altair).some
        case BasicConfiguration.GhostIfu(_, _, _, _, _)                                      =>
          AgsParams.GhostIfu().some
        case BasicConfiguration.Visitor(agsDiameter = ags, scienceFovDiameter = fov)         =>
          AgsParams.Visitor(ags, fov, port).some
        case BasicConfiguration.KeckExchange(_, _) | BasicConfiguration.SubaruExchange(_, _) =>
          none

    // Re-selecting a PWFS would drop the Altair mode `guidedBy` already applied.
    if base.exists(_.altair.isDefined) then base
    else
      guideProbe match
        case Some(GuideProbe.PWFS1) => base.map(_.withPWFS1)
        case Some(GuideProbe.PWFS2) => base.map(_.withPWFS2)
        case _                      => base
