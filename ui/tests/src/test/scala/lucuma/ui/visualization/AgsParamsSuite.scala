// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.syntax.all.*
import lucuma.ags.AgsParams
import lucuma.ags.SingleProbeAgsParams
import lucuma.core.enums.AltairMode
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.PortDisposition
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.arb.ArbBasicConfiguration.given
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

class AgsParamsSuite extends ScalaCheckSuite:

  private def params(
    conf:       BasicConfiguration,
    guideProbe: GuideProbe,
    altair:     Option[AltairMode]
  ): Option[AgsParams & SingleProbeAgsParams] =
    conf.agsParams(PortDisposition.Side, guideProbe.some, altair)

  private def checkAltair(conf: BasicConfiguration, mode: AltairMode): Unit =
    val result: Option[AgsParams & SingleProbeAgsParams] = params(conf, mode.guideProbe, mode.some)
    assertEquals(result.map(_.probe), mode.guideProbe.some)
    assertEquals(result.flatMap(_.altair), mode.some)

  private def checkPwfs(conf: BasicConfiguration, mode: AltairMode): Unit =
    val result: Option[AgsParams & SingleProbeAgsParams] = params(conf, GuideProbe.PWFS2, mode.some)
    assertEquals(result.map(_.probe), GuideProbe.PWFS2.some)
    assertEquals(result.flatMap(_.altair), none)

  property("GNIRS spectroscopy behind Altair uses the Altair mode's probe"):
    forAll: (conf: BasicConfiguration.GnirsSpectroscopy, mode: AltairMode) =>
      checkAltair(conf, mode)

  property("GNIRS imaging behind Altair uses the Altair mode's probe"):
    forAll: (conf: BasicConfiguration.GnirsImaging, mode: AltairMode) =>
      checkAltair(conf, mode)

  property("GNIRS spectroscopy on another PWFS guides without Altair"):
    forAll: (conf: BasicConfiguration.GnirsSpectroscopy, mode: AltairMode) =>
      checkPwfs(conf, mode)

  property("GNIRS imaging on another PWFS guides without Altair"):
    forAll: (conf: BasicConfiguration.GnirsImaging, mode: AltairMode) =>
      checkPwfs(conf, mode)

  property("LGS+P1 guides with PWFS1 and keeps the Altair mode"):
    forAll: (conf: BasicConfiguration.GnirsSpectroscopy) =>
      val result: Option[AgsParams & SingleProbeAgsParams] =
        params(conf, GuideProbe.PWFS1, AltairMode.LgsP1.some)
      assertEquals(result.map(_.probe), GuideProbe.PWFS1.some)
      assertEquals(result.flatMap(_.altair), AltairMode.LgsP1.some)

  property("GNIRS without Altair keeps the plain PWFS params"):
    forAll: (conf: BasicConfiguration.GnirsSpectroscopy) =>
      val result: Option[AgsParams & SingleProbeAgsParams] =
        params(conf, GuideProbe.PWFS1, none)
      assertEquals(result.map(_.probe), GuideProbe.PWFS1.some)
      assertEquals(result.flatMap(_.altair), none)
