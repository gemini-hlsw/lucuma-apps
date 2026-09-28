// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import explore.model.syntax.all.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.AltairNdFilter
import lucuma.core.enums.CassRotator
import lucuma.core.enums.FieldLens
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.util.Display
import lucuma.odb.data.AltairConfiguration
import munit.FunSuite

class GuiderChoiceSuite extends FunSuite:
  private val pwfsChoices: List[GuiderChoice] =
    List(GuiderChoice.Probe(GuideProbe.PWFS2), GuiderChoice.Probe(GuideProbe.PWFS1))

  private val altairChoices: List[GuiderChoice] =
    List(
      GuiderChoice.Altair(AltairMode.Ngs),
      GuiderChoice.Altair(AltairMode.Lgs),
      GuiderChoice.Altair(AltairMode.LgsP1)
    )

  private val ngsWithOptions: AltairConfiguration =
    AltairConfiguration(AltairMode.Ngs, FieldLens.Out.some, CassRotator.Fixed, AltairNdFilter.In)

  List(
    ObservingModeType.GnirsLongSlit,
    ObservingModeType.GnirsIfu,
    ObservingModeType.GnirsImaging
  ).foreach: mode =>
    test(s"options for $mode list the Altair modes after the probes"):
      assertEquals(GuiderChoice.options(mode), pwfsChoices ++ altairChoices)

  List(
    ObservingModeType.GmosNorthLongSlit,
    ObservingModeType.GmosSouthImaging,
    ObservingModeType.Flamingos2LongSlit
  ).foreach: mode =>
    test(s"options for $mode have no Altair modes"):
      val options: List[GuiderChoice] = GuiderChoice.options(mode)
      assert(options.nonEmpty)
      assert(options.forall(!altairChoices.contains(_)))

  test("options for GMOS include the OIWFS"):
    assert(
      GuiderChoice
        .options(ObservingModeType.GmosNorthLongSlit)
        .contains(GuiderChoice.Probe(GuideProbe.GmosOIWFS))
    )

  test("current is empty with neither a probe nor Altair"):
    assertEquals(GuiderChoice.current(GuidingConfiguration.Empty), none)

  test("current is the explicit probe without Altair"):
    assertEquals(
      GuiderChoice.current(GuidingConfiguration(GuideProbe.PWFS1.some, none)),
      GuiderChoice.Probe(GuideProbe.PWFS1).some
    )

  test("current is the Altair mode when Altair is set, whatever the explicit probe"):
    val lgs: AltairConfiguration = AltairConfiguration.default(AltairMode.Lgs)
    assertEquals(GuiderChoice.current(GuidingConfiguration(none, lgs.some)),
                 GuiderChoice.Altair(AltairMode.Lgs).some
    )
    assertEquals(
      GuiderChoice.current(GuidingConfiguration(GuideProbe.PWFS2.some, lgs.some)),
      GuiderChoice.Altair(AltairMode.Lgs).some
    )

  test("display"):
    assertEquals(
      (pwfsChoices ++ altairChoices).map(Display[GuiderChoice].shortName),
      List("PWFS2", "PWFS1", "Altair NGS", "Altair LGS", "Altair LGS+P1")
    )

  test("selecting a probe clears Altair"):
    assertEquals(
      GuiderChoice.select(GuiderChoice.Probe(GuideProbe.PWFS1).some, ngsWithOptions.some),
      GuidingConfiguration(GuideProbe.PWFS1.some, none)
    )

  test("clearing the selection clears both"):
    assertEquals(GuiderChoice.select(none, ngsWithOptions.some), GuidingConfiguration.Empty)

  test("selecting Altair without a configuration uses the default"):
    assertEquals(
      GuiderChoice.select(GuiderChoice.Altair(AltairMode.LgsP1).some, none),
      GuidingConfiguration(none, AltairConfiguration.default(AltairMode.LgsP1).some)
    )

  test("changing the Altair mode keeps the rest of the configuration"):
    val lgsP1: AltairConfiguration =
      AltairConfiguration(AltairMode.LgsP1,
                          FieldLens.In.some,
                          CassRotator.Fixed,
                          AltairNdFilter.Out
      )
    assertEquals(
      GuiderChoice.select(GuiderChoice.Altair(AltairMode.Ngs).some, lgsP1.some),
      GuidingConfiguration(none, lgsP1.copy(mode = AltairMode.Ngs).some)
    )

  test("changing to a laser mode drops the settings it cannot use"):
    assertEquals(
      GuiderChoice.select(GuiderChoice.Altair(AltairMode.Lgs).some, ngsWithOptions.some),
      GuidingConfiguration(
        none,
        AltairConfiguration(AltairMode.Lgs, none, CassRotator.Fixed, AltairNdFilter.Out).some
      )
    )
