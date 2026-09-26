// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import munit.FunSuite

class ConfigurationSummarySuite extends FunSuite:
  private val gnirsSlit: BasicConfiguration =
    BasicConfiguration.GnirsSpectroscopy(
      GnirsFilter.Order4,
      GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_30),
      GnirsPrism.Sxd,
      GnirsGrating.D32,
      GnirsCamera.ShortBlue,
      CentralWavelength(Wavelength.fromIntNanometers(2230).get)
    )

  test("no suffix without Altair"):
    assertEquals(
      display.configurationSummary(gnirsSlit, none),
      "SB 32 l/mm @ 2.23µm SXD 0.30\" slit"
    )

  test("Altair mode is appended"):
    assertEquals(
      display.configurationSummary(gnirsSlit, AltairMode.Ngs.some),
      "SB 32 l/mm @ 2.23µm SXD 0.30\" slit AO:NGS"
    )
    assertEquals(
      display.configurationSummary(gnirsSlit, AltairMode.Lgs.some),
      "SB 32 l/mm @ 2.23µm SXD 0.30\" slit AO:LGS"
    )
    assertEquals(
      display.configurationSummary(gnirsSlit, AltairMode.LgsP1.some),
      "SB 32 l/mm @ 2.23µm SXD 0.30\" slit AO:LGS+P1"
    )
