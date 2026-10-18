// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.config

import com.typesafe.config.Config
import observe.model.config.SubsystemControlModes
import observe.model.enums.SubsystemControlMode
import pureconfig.*

/**
 * Checks that the subsystem control modes are read from `observe-engine.subsystem-control-modes`,
 * and from the deprecated `observe-engine.system-control` key.
 */
class SubsystemControlModesKeySuite extends munit.FunSuite:

  private val AllModes: List[String] =
    List("altair",
         "gems",
         "dhs",
         "flamingos-2",
         "gcal",
         "ghost",
         "gmos",
         "gnirs",
         "gpi",
         "gsaoi",
         "gws",
         "nifs",
         "niri",
         "tcs",
         "igrins-2"
    )

  private def block(key: String, mode: String, tcsMode: String): String =
    val entries: String = AllModes
      .map(m => s"$m = ${if m == "tcs" then tcsMode else mode}")
      .mkString("\n")
    s"observe-engine { $key { $entries } }"

  private def parse(hocon: String): Config =
    ConfigSource.string(hocon).config().fold(f => fail(f.prettyPrint()), identity)

  private def modes(config: Config): SubsystemControlModes =
    ConfigSource
      .fromConfig(migrateSystemControl(config))
      .at(SubsystemControlModesKey)
      .loadOrThrow[SubsystemControlModes]

  private def expected(mode: SubsystemControlMode, tcsMode: SubsystemControlMode) =
    SubsystemControlModes(
      altair = mode,
      gems = mode,
      dhs = mode,
      flamingos2 = mode,
      gcal = mode,
      gmos = mode,
      gnirs = mode,
      gpi = mode,
      ghost = mode,
      igrins2 = mode,
      gsaoi = mode,
      gws = mode,
      nifs = mode,
      niri = mode,
      tcs = tcsMode
    )

  test("the new key loads unchanged"):
    val config: Config = parse(block("subsystem-control-modes", "simulated", "full"))
    assert(!hasDeprecatedSystemControl(config))
    assertEquals(migrateSystemControl(config), config)
    assertEquals(
      modes(config),
      expected(SubsystemControlMode.Simulated, SubsystemControlMode.Full)
    )

  test("the deprecated key alone loads"):
    val config: Config = parse(block("system-control", "readOnly", "full"))
    assert(hasDeprecatedSystemControl(config))
    assertEquals(modes(config), expected(SubsystemControlMode.ReadOnly, SubsystemControlMode.Full))

  test("the deprecated key is removed once migrated"):
    val config: Config = migrateSystemControl(parse(block("system-control", "simulated", "full")))
    assert(!hasDeprecatedSystemControl(config))
    assert(config.hasPath(SubsystemControlModesKey))

  test("a partial deprecated key overrides the defaults under the new key"):
    val defaults: String = block("subsystem-control-modes", "simulated", "simulated")
    val config: Config   =
      ConfigSource
        .string("observe-engine.system-control.tcs = full")
        .withFallback(ConfigSource.string(defaults))
        .config()
        .fold(f => fail(f.prettyPrint()), identity)
    assertEquals(
      modes(config),
      expected(SubsystemControlMode.Simulated, SubsystemControlMode.Full)
    )

  test("a deprecated key overrides the defaults in base.conf"):
    val config: Config                =
      ConfigSource
        .string("observe-engine.system-control.tcs = full")
        .withFallback(ConfigSource.resources("base.conf"))
        .config()
        .fold(f => fail(f.prettyPrint()), identity)
    val loaded: SubsystemControlModes = modes(config)
    assertEquals(loaded.tcs, SubsystemControlMode.Full)
    assertEquals(loaded.gcal, SubsystemControlMode.Simulated)
