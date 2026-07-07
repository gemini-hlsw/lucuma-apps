// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.Semigroup
import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^.VdomAttr
import lucuma.ags.AgsParams
import lucuma.ags.GuideStarCandidate
import lucuma.ags.SingleProbeAgsParams
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.SequenceType
import lucuma.core.enums.TrackType
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.util.NewBoolean
import lucuma.react.common.Css
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.aladin.Fov
import org.locationtech.jts.geom.Geometry

import scala.math.*

val canvasWidth      = VdomAttr("width")
val canvasHeight     = VdomAttr("height")
val patternUnits     = VdomAttr("patternUnits")
val patternTransform = VdomAttr("patternTransform")

// The values on the geometry are in microarcseconds
// They are fairly large and break is some browsers
// We apply a scaling factor uniformil
inline def scale: Double => Double = (v: Double) => rint(v / 1000)

inline def reverseScale: Double => Double = (v: Double) => rint(v * 1000)

val geometryUnionSemigroup: Semigroup[Geometry] =
  Semigroup.instance(_.union(_))

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
  def agsParams(
    port:      PortDisposition,
    trackType: Option[TrackType]
  ): Option[AgsParams & SingleProbeAgsParams] =
    val base =
      conf match
        case BasicConfiguration.GmosNorthLongSlit(fpu = fpu)                                 =>
          AgsParams.GmosLongSlit(fpu.asLeft, port).some
        case BasicConfiguration.GmosSouthLongSlit(fpu = fpu)                                 =>
          AgsParams.GmosLongSlit(fpu.asRight, port).some
        case BasicConfiguration.GmosNorthImaging(_)                                          =>
          AgsParams.GmosImaging(port).some
        case BasicConfiguration.GmosSouthImaging(_)                                          =>
          AgsParams.GmosImaging(port).some
        case BasicConfiguration.Flamingos2LongSlit(fpu = fpu)                                =>
          AgsParams
            .Flamingos2LongSlit(Flamingos2LyotWheel.F16, Flamingos2FpuMask.Builtin(fpu), port)
            .some
        case BasicConfiguration.Flamingos2Imaging(_)                                         =>
          AgsParams.Flamingos2Imaging(Flamingos2LyotWheel.F16, port).some
        case BasicConfiguration.Igrins2LongSlit                                              =>
          AgsParams.Igrins2LongSlit().some
        case BasicConfiguration.GnirsImaging(camera = camera)                                =>
          AgsParams.GnirsImaging(camera, port).some
        case BasicConfiguration.GnirsSpectroscopy(fpu = GnirsFpu.Spectroscopy.Ifu(ifu))      =>
          AgsParams.GnirsIfu(ifu, port).some
        case BasicConfiguration.GnirsSpectroscopy(fpu = fpu, prism = prism, camera = camera) =>
          // Slit (or, defensively, any non-IFU fpu) → long-slit probe params.
          val slit = GnirsFpu.Spectroscopy.slit.getOption(fpu).getOrElse(GnirsFpuSlit.LongSlit_1_00)
          AgsParams.GnirsLongSlit(slit, camera, prism, port).some
        case BasicConfiguration.GhostIfu(_, _, _, _, _)                                      =>
          AgsParams.GhostIfu().some
        case BasicConfiguration.Visitor(mode = VisitorObservingModeType.MaroonX)             =>
          AgsParams.MaroonX(port).some
        case BasicConfiguration.Visitor(agsDiameter = fov)                                   =>
          AgsParams.Visitor(fov, port).some
        case BasicConfiguration.KeckExchange(_, _) | BasicConfiguration.SubaruExchange(_, _) =>
          none

    conf.guideProbe(trackType) match
      case Some(GuideProbe.PWFS1) => base.map(_.withPWFS1)
      case Some(GuideProbe.PWFS2) => base.map(_.withPWFS2)
      case _                      => base
