// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.tcs

import cats.effect.*
import cats.syntax.all.*
import edu.gemini.epics.acm.*
import lucuma.core.math.Angle
import lucuma.core.util.TimeSpan
import observe.model.enums.ApplyCommandResult
import observe.server.EpicsCommand
import observe.server.EpicsCommandBase
import observe.server.EpicsSystem
import observe.server.EpicsUtil.*

/**
 * TcsEpics wraps the non-functional parts of the EPICS ACM library to interact with TCS. It has all
 * the objects used to read TCS status values and execute TCS commands.
 *
 * Created by jluhrs on 10/1/15.
 */

trait TcsEpics[F[_]] {

  import TcsEpics.*

  def post(timeout: TimeSpan): F[ApplyCommandResult]

  val observe: EpicsCommand[F]

  val endObserve: EpicsCommand[F]

  def instrAA: F[Double]

  def hourAngle: F[String]

  def localTime: F[String]

  def trackingFrame: F[String]

  def trackingEpoch: F[Double]

  def equinox: F[Double]

  def trackingEquinox: F[String]

  def trackingDec: F[Double]

  def trackingRA: F[Double]

  def elevation: F[Double]

  def azimuth: F[Double]

  def crPositionAngle: F[Double]

  def ut: F[String]

  def date: F[String]

  def m2Baffle: F[String]

  def m2CentralBaffle: F[String]

  def st: F[String]

  def sfRotation: F[Double]

  def sfTilt: F[Double]

  def sfLinear: F[Double]

  def instrPA: F[Double]

  def targetA: F[List[Double]]

  def aoFoldPosition: F[String]

  def airmass: F[Double]

  def airmassStart: F[Double]

  def airmassEnd: F[Double]

  def carouselMode: F[String]

  def crFollow: F[Int]

  def crTrackingFrame: F[String]

  def sourceATarget: Target[F]

  val pwfs1Target: Target[F]

  val pwfs2Target: Target[F]

  val oiwfsTarget: Target[F]

  def parallacticAngle: F[Angle]

  def m2UserFocusOffset: F[Double]

  def pwfs1IntegrationTime: F[Double]

  def pwfs2IntegrationTime: F[Double]

  // Attribute must be changed back to Double after EPICS channel is fixed.
  def oiwfsIntegrationTime: F[Double]

  def gsaoiPort: F[Int]

  def gpiPort: F[Int]

  def f2Port: F[Int]

  def niriPort: F[Int]

  def gnirsPort: F[Int]

  def nifsPort: F[Int]

  def gmosPort: F[Int]

  def ghostPort: F[Int]

  def igrins2Port: F[Int]

  def sourceAWavelengthAngstroms: F[Double]

  def gwfs1Target: Target[F]

  def gwfs2Target: Target[F]

  def gwfs3Target: Target[F]

  def gwfs4Target: Target[F]

  import VirtualGemsTelescope.*

  def gemsTarget(g: VirtualGemsTelescope): Target[F] = g match {
    case G1 => gwfs1Target
    case G2 => gwfs2Target
    case G3 => gwfs3Target
    case G4 => gwfs4Target
  }

  def g1MapName: F[Option[GemsSource]]

  def g2MapName: F[Option[GemsSource]]

  def g3MapName: F[Option[GemsSource]]

  def g4MapName: F[Option[GemsSource]]

}

final class TcsEpicsImpl[F[_]: Async](epicsService: CaService) extends TcsEpics[F] {

  import TcsEpics.*

  // Commands are triggered from the main apply record, which all TCS commands share, so any of them
  // can be used to post.
  override def post(timeout: TimeSpan): F[ApplyCommandResult] = observe.post(timeout)

  override val observe: EpicsCommand[F] = new EpicsCommandBase[F](sysName) {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("tcs::observe"))
  }

  override val endObserve: EpicsCommand[F] = new EpicsCommandBase[F](sysName) {
    override val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("tcs::endObserve")
    )
  }

  private val tcsState = epicsService.getStatusAcceptor("tcsstate")

  override def instrAA: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("instrAA"))

  override def hourAngle: F[String] = safeAttributeF(tcsState.getStringAttribute("ha"))

  override def localTime: F[String] = safeAttributeF(tcsState.getStringAttribute("lt"))

  override def trackingFrame: F[String] = safeAttributeF(tcsState.getStringAttribute("trkframe"))

  override def trackingEpoch: F[Double] = safeAttributeSDoubleF(
    tcsState.getDoubleAttribute("trkepoch")
  )

  override def equinox: F[Double] = safeAttributeSDoubleF(
    tcsState.getDoubleAttribute("sourceAEquinox")
  )

  override def trackingEquinox: F[String] = safeAttributeF(
    tcsState.getStringAttribute("sourceATrackEq")
  )

  override def trackingDec: F[Double] = safeAttributeSDoubleF(
    tcsState.getDoubleAttribute("dectrack")
  )

  override def trackingRA: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("ratrack"))

  override def elevation: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("elevatio"))

  override def azimuth: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("azimuth"))

  override def crPositionAngle: F[Double] = safeAttributeSDoubleF(
    tcsState.getDoubleAttribute("crpa")
  )

  override def ut: F[String] = safeAttributeF(tcsState.getStringAttribute("ut"))

  override def date: F[String] = safeAttributeF(tcsState.getStringAttribute("date"))

  override def m2Baffle: F[String] = safeAttributeF(tcsState.getStringAttribute("m2baffle"))

  override def m2CentralBaffle: F[String] = safeAttributeF(tcsState.getStringAttribute("m2cenbaff"))

  override def st: F[String] = safeAttributeF(tcsState.getStringAttribute("st"))

  override def sfRotation: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sfrt2"))

  override def sfTilt: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sftilt"))

  override def sfLinear: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sflinear"))

  override def instrPA: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("instrPA"))

  override def targetA: F[List[Double]] = safeAttributeSListSDoubleF(
    tcsState.getDoubleAttribute("targetA")
  )

  override def aoFoldPosition: F[String] = safeAttributeF(tcsState.getStringAttribute("aoName"))

  override def airmass: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("airmass"))

  override def airmassStart: F[Double] = safeAttributeSDoubleF(
    tcsState.getDoubleAttribute("amstart")
  )

  override def airmassEnd: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("amend"))

  override def carouselMode: F[String] = safeAttributeF(tcsState.getStringAttribute("cguidmod"))

  override def crFollow: F[Int] = safeAttributeSIntF(tcsState.getIntegerAttribute("crfollow"))

  override def crTrackingFrame: F[String] = safeAttributeF(
    tcsState.getStringAttribute("rotTrackFrame")
  )

  override def sourceATarget: Target[F] = new Target[F] {
    override def epoch: F[String] = safeAttributeF(tcsState.getStringAttribute("sourceAEpoch"))

    override def equinox: F[String] = safeAttributeF(tcsState.getStringAttribute("sourceAEquinox"))

    override def radialVelocity: F[Double] = safeAttributeSDoubleF(
      tcsState.getDoubleAttribute("radvel")
    )

    override def frame: F[String] = safeAttributeF(tcsState.getStringAttribute("frame"))

    override def centralWavelengthAngstroms: F[Double] = sourceAWavelengthAngstroms

    override def ra: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("ra"))

    override def objectName: F[String] = safeAttributeF(
      tcsState.getStringAttribute("sourceAObjectName")
    )

    override def dec: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("dec"))

    override def parallax: F[Double] = safeAttributeSDoubleF(
      tcsState.getDoubleAttribute("parallax")
    )

    override def properMotionRA: F[Double] = safeAttributeSDoubleF(
      tcsState.getDoubleAttribute("pmra")
    )

    override def properMotionDec: F[Double] = safeAttributeSDoubleF(
      tcsState.getDoubleAttribute("pmdec")
    )
  }

  private def target(base: String): Target[F] = new Target[F] {
    override def epoch: F[String]                      = safeAttributeF(tcsState.getStringAttribute(base + "aepoch"))
    override def equinox: F[String]                    = safeAttributeF(tcsState.getStringAttribute(base + "aequin"))
    override def radialVelocity: F[Double]             = safeAttributeSDoubleF(
      tcsState.getDoubleAttribute(base + "arv")
    )
    override def frame: F[String]                      = safeAttributeF(tcsState.getStringAttribute(base + "aframe"))
    override def centralWavelengthAngstroms: F[Double] =
      safeAttributeSDoubleF(tcsState.getDoubleAttribute(base + "awavel"))
    override def ra: F[Double]                         = safeAttributeSDoubleF(tcsState.getDoubleAttribute(base + "ara"))
    override def objectName: F[String]                 = safeAttributeF(
      tcsState.getStringAttribute(base + "aobjec")
    )
    override def dec: F[Double]                        = safeAttributeSDoubleF(tcsState.getDoubleAttribute(base + "adec"))
    override def parallax: F[Double]                   = safeAttributeSDoubleF(
      tcsState.getDoubleAttribute(base + "aparal")
    )
    override def properMotionRA: F[Double]             = safeAttributeSDoubleF(
      tcsState.getDoubleAttribute(base + "apmra")
    )
    override def properMotionDec: F[Double]            =
      safeAttributeSDoubleF(tcsState.getDoubleAttribute(base + "apmdec"))
  }

  override val pwfs1Target: Target[F] = target("p1")

  override val pwfs2Target: Target[F] = target("p2")

  override val oiwfsTarget: Target[F] = target("oi")

  override def parallacticAngle: F[Angle] =
    safeAttributeSDoubleF(tcsState.getDoubleAttribute("parangle")).map(Angle.fromDoubleDegrees(_))

  override def m2UserFocusOffset: F[Double] = safeAttributeSDoubleF(
    tcsState.getDoubleAttribute("m2ZUserOffset")
  )

  private val pwfs1Status = epicsService.getStatusAcceptor("pwfs1state")

  override def pwfs1IntegrationTime: F[Double] = safeAttributeSDoubleF(
    pwfs1Status.getDoubleAttribute("intTime")
  )

  private val pwfs2Status = epicsService.getStatusAcceptor("pwfs2state")

  override def pwfs2IntegrationTime: F[Double] = safeAttributeSDoubleF(
    pwfs2Status.getDoubleAttribute("intTime")
  )

  private val oiwfsStatus = epicsService.getStatusAcceptor("oiwfsstate")

  // Attribute must be changed back to Double after EPICS channel is fixed.
  override def oiwfsIntegrationTime: F[Double] = safeAttributeSDoubleF(
    oiwfsStatus.getDoubleAttribute("intTime")
  )

  private def instPort(name: String): F[Int] =
    safeAttributeSIntF(tcsState.getIntegerAttribute(s"${name}Port"))

  override def gsaoiPort: F[Int]   = instPort("gsaoi")
  override def gpiPort: F[Int]     = instPort("gpi")
  override def f2Port: F[Int]      = instPort("f2")
  override def niriPort: F[Int]    = instPort("niri")
  override def gnirsPort: F[Int]   = instPort("nirs")
  override def nifsPort: F[Int]    = instPort("nifs")
  override def gmosPort: F[Int]    = instPort("gmos")
  override def ghostPort: F[Int]   = instPort("ghost")
  override def igrins2Port: F[Int] = instPort("igrins2")

  override def sourceAWavelengthAngstroms: F[Double] = safeAttributeSDoubleF(
    tcsState.getDoubleAttribute("sourceAWavelength")
  )

  override def gwfs1Target: Target[F] = target("g1")

  override def gwfs2Target: Target[F] = target("g2")

  override def gwfs3Target: Target[F] = target("g3")

  override def gwfs4Target: Target[F] = target("g4")

  override def g1MapName: F[Option[GemsSource]] =
    safeAttributeF(tcsState.getStringAttribute("g1MapName"))
      .map(x => GemsSource.all.find(_.epicsVal === x))

  override def g2MapName: F[Option[GemsSource]] =
    safeAttributeF(tcsState.getStringAttribute("g2MapName"))
      .map(x => GemsSource.all.find(_.epicsVal === x))

  override def g3MapName: F[Option[GemsSource]] =
    safeAttributeF(tcsState.getStringAttribute("g3MapName"))
      .map(x => GemsSource.all.find(_.epicsVal === x))

  override def g4MapName: F[Option[GemsSource]] =
    safeAttributeF(tcsState.getStringAttribute("g4MapName"))
      .map(x => GemsSource.all.find(_.epicsVal === x))

}

object TcsEpics extends EpicsSystem[TcsEpics[IO]] {

  val sysName: String = "TCS"

  override val className: String      = getClass.getName
  override val CA_CONFIG_FILE: String = "/Tcs.xml"

  override def build[F[_]: Sync](service: CaService, tops: Map[String, String]): F[TcsEpics[IO]] =
    Sync[F].delay(new TcsEpicsImpl[IO](service))

  trait Target[F[_]] {
    def objectName: F[String]
    def ra: F[Double]
    def dec: F[Double]
    def frame: F[String]
    def equinox: F[String]
    def epoch: F[String]
    def properMotionRA: F[Double]
    def properMotionDec: F[Double]
    def centralWavelengthAngstroms: F[Double]
    def parallax: F[Double]
    def radialVelocity: F[Double]
  }

  // TODO: Delete me after fully moved to tagless
  extension (tio: Target[IO]) {
    def to[F[_]: LiftIO]: Target[F] = new Target[F] {
      def objectName: F[String]                 = tio.objectName.to[F]
      def ra: F[Double]                         = tio.ra.to[F]
      def dec: F[Double]                        = tio.dec.to[F]
      def frame: F[String]                      = tio.frame.to[F]
      def equinox: F[String]                    = tio.equinox.to[F]
      def epoch: F[String]                      = tio.epoch.to[F]
      def properMotionRA: F[Double]             = tio.properMotionRA.to[F]
      def properMotionDec: F[Double]            = tio.properMotionDec.to[F]
      def centralWavelengthAngstroms: F[Double] = tio.centralWavelengthAngstroms.to[F]
      def parallax: F[Double]                   = tio.parallax.to[F]
      def radialVelocity: F[Double]             = tio.radialVelocity.to[F]
    }
  }

  sealed trait VirtualGemsTelescope extends Product with Serializable
  object VirtualGemsTelescope {
    case object G1 extends VirtualGemsTelescope
    case object G2 extends VirtualGemsTelescope
    case object G3 extends VirtualGemsTelescope
    case object G4 extends VirtualGemsTelescope
  }

}
