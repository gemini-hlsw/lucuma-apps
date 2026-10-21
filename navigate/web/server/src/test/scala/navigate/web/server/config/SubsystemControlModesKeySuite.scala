// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.config

import cats.effect.IO
import com.typesafe.config.Config
import navigate.model.config.NavigateConfiguration
import navigate.model.config.SubsystemControlMode
import navigate.model.config.SubsystemControlModes
import pureconfig.ConfigSource

/**
 * Checks that the subsystem control modes are read from `navigate-engine.subsystem-control-modes`,
 * and from the deprecated `navigate-engine.system-control` key.
 */
class SubsystemControlModesKeySuite extends munit.CatsEffectSuite:

  private def withBase(hocon: String): Config =
    ConfigSource
      .string(hocon)
      .withFallback(ConfigSource.resources("base.conf"))
      .config()
      .fold(f => fail(f.prettyPrint()), identity)

  private def modes(config: Config): IO[SubsystemControlModes] =
    loadConfiguration[IO](ConfigSource.fromConfig(migrateSystemControl(config)))
      .map((c: NavigateConfiguration) => c.navigateEngine.subsystemControlModes)

  test("base.conf loads with the new key"):
    val config: Config = withBase("")
    assert(!hasDeprecatedSystemControl(config))
    assertEquals(migrateSystemControl(config), config)
    modes(config).map(m => assertEquals(m, SubsystemControlModes.default))

  test("the new key overrides base.conf"):
    modes(withBase("navigate-engine.subsystem-control-modes.tcs = full")).map: m =>
      assertEquals(m.tcs, SubsystemControlMode.Full)
      assertEquals(m.gcal, SubsystemControlMode.Simulated)

  test("the deprecated key overrides base.conf"):
    val config: Config = withBase("navigate-engine.system-control.tcs = readOnly")
    assert(hasDeprecatedSystemControl(config))
    modes(config).map: m =>
      assertEquals(m.tcs, SubsystemControlMode.ReadOnly)
      assertEquals(m.observe, SubsystemControlMode.Simulated)

  test("the deprecated key is removed once migrated"):
    val config: Config = migrateSystemControl(withBase("navigate-engine.system-control.tcs = full"))
    assert(!hasDeprecatedSystemControl(config))
    assertEquals(config.getString(s"$SubsystemControlModesKey.tcs"), "full")
