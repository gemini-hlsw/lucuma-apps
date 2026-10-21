// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.config

import cats.kernel.laws.discipline.*
import io.circe.testing.CodecTests
import io.circe.testing.instances.*
import lucuma.core.enums.Site
import observe.model.config.arb.ArbSubsystemControlModes.given
import observe.model.enums.SubsystemControlMode
import org.scalacheck.Arbitrary

/**
 * Tests config classes
 */
class ConfigModelSuite extends munit.DisciplineSuite {
  checkAll("Eq[SubsystemControlModes]", EqTests[SubsystemControlModes].eqv)
  checkAll("Codec[SubsystemControlModes]", CodecTests[SubsystemControlModes].unserializableCodec)

  test("fieldsFor lists common fields first and no GPI") {
    val gs = SubsystemControlModes.fieldsFor(Site.GS).map(_.configKey)
    val gn = SubsystemControlModes.fieldsFor(Site.GN).map(_.configKey)
    assertEquals(gs.take(5), List("tcs", "gcal", "dhs", "gws", "gmos"))
    assertEquals(gs.drop(5).toSet, Set("gems", "gsaoi", "flamingos-2", "ghost"))
    assertEquals(gn.drop(5).toSet, Set("altair", "gnirs", "niri", "nifs", "igrins-2"))
  }

  test("diff only reports the fields of the given site") {
    val a = summon[Arbitrary[SubsystemControlModes]].arbitrary.sample.get
    val b = a
      .copy(gmos = SubsystemControlMode.Simulated, gems = SubsystemControlMode.Simulated)
      .copy(altair = SubsystemControlMode.Full)
    val c = b.copy(
      gmos = SubsystemControlMode.Full,
      gems = SubsystemControlMode.Full,
      altair = SubsystemControlMode.Simulated
    )
    assertEquals(b.diff(c, Site.GS).map(_._1.configKey).toSet, Set("gmos", "gems"))
    assertEquals(b.diff(c, Site.GN).map(_._1.configKey).toSet, Set("gmos", "altair"))
    assertEquals(b.diff(b, Site.GS), Nil)
  }
}
