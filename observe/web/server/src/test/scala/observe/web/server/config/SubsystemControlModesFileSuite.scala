// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.config

import cats.effect.IO
import com.typesafe.config.ConfigFactory
import lucuma.core.enums.Site
import observe.model.config.SubsystemControlModes
import observe.model.enums.SubsystemControlMode
import pureconfig.*

import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters.*

class SubsystemControlModesFileSuite extends munit.CatsEffectSuite:
  private val ConfigPath: String = "observe-engine.subsystem-control-modes"

  private val AllFull: SubsystemControlModes =
    SubsystemControlModes(
      altair = SubsystemControlMode.Full,
      gems = SubsystemControlMode.Full,
      dhs = SubsystemControlMode.Full,
      flamingos2 = SubsystemControlMode.Full,
      gcal = SubsystemControlMode.ReadOnly,
      gmos = SubsystemControlMode.Full,
      gnirs = SubsystemControlMode.Full,
      gpi = SubsystemControlMode.Full,
      ghost = SubsystemControlMode.Full,
      igrins2 = SubsystemControlMode.ReadOnly,
      gsaoi = SubsystemControlMode.Full,
      gws = SubsystemControlMode.Simulated,
      nifs = SubsystemControlMode.Full,
      niri = SubsystemControlMode.Full,
      tcs = SubsystemControlMode.Full
    )

  private val AllSimulated: SubsystemControlModes =
    (SubsystemControlModes.fieldsFor(Site.GN) ++ SubsystemControlModes.fieldsFor(Site.GS))
      .foldLeft(AllFull): (c, f) =>
        f.mode.replace(SubsystemControlMode.Simulated)(c)

  private val TempDir: FunFixture[Path] =
    FunFixture[Path](
      _ => Files.createTempDirectory("systems-control"),
      dir =>
        Files.list(dir).iterator.asScala.foreach(Files.deleteIfExists(_))
        val _ = Files.deleteIfExists(dir)
    )

  private def load(file: Path): IO[SubsystemControlModes] =
    IO.delay:
      ConfigSource
        .file(file)
        .withFallback(ConfigSource.resources("base.conf"))
        .at(ConfigPath)
        .loadOrThrow[SubsystemControlModes]

  private def writtenKeys(file: Path): Set[String] =
    ConfigFactory.parseFile(file.toFile).getObject(ConfigPath).keySet.asScala.toSet

  private def writtenValue(file: Path, key: String): String =
    ConfigFactory.parseFile(file.toFile).getString(s"$ConfigPath.$key")

  private def expectedKeys(site: Site): Set[String] =
    SubsystemControlModes.fieldsFor(site).map(_.configKey).toSet

  // The configuration as the server reads it back: site fields as written, the rest simulated.
  private def expectedLoaded(
    site:    Site,
    written: SubsystemControlModes
  ): SubsystemControlModes =
    SubsystemControlModes
      .fieldsFor(site)
      .foldLeft(AllSimulated.copy(gpi = SubsystemControlMode.Simulated)): (c, f) =>
        f.mode.replace(f.mode.get(written))(c)

  List(Site.GS, Site.GN).foreach: site =>
    TempDir.test(s"writes exactly the $site keys, explicit, and reads them back"): dir =>
      val file = dir.resolve("subsystems.conf")
      for
        _      <- SubsystemControlModesFile.write[IO](file, site, AllFull, "tester")
        loaded <- load(file)
      yield
        assertEquals(writtenKeys(file), expectedKeys(site))
        // Keys are the ones pureconfig derives for the case class fields
        assertEquals(writtenValue(file, "gws"), "simulated")
        assertEquals(writtenValue(file, "gcal"), "readOnly")
        assertEquals(writtenValue(file, "tcs"), "full")
        assertEquals(loaded, expectedLoaded(site, AllFull))

  TempDir.test("keys derived by the reader match the written ones (flamingos-2, igrins-2)"): dir =>
    val file = dir.resolve("subsystems.conf")
    for
      _  <- SubsystemControlModesFile.write[IO](file, Site.GS, AllFull, "tester")
      gs <- load(file)
      _  <- SubsystemControlModesFile.write[IO](file, Site.GN, AllFull, "tester")
      gn <- load(file)
    yield
      assertEquals(writtenKeys(file).contains("igrins-2"), true)
      assertEquals(gn.igrins2, SubsystemControlMode.ReadOnly)
      assertEquals(gs.flamingos2, SubsystemControlMode.Full)
      assertEquals(gs.igrins2, SubsystemControlMode.Simulated)

  TempDir.test("unrelated keys survive and rewriting is idempotent"): dir =>
    val file = dir.resolve("subsystems.conf")
    Files.writeString(
      file,
      """observe-engine { odb-queue-polling-interval = 7 seconds
        |  subsystem-control-modes { tcs = readOnly, gems = full } }
        |other { key = "value" }
        |""".stripMargin
    )
    for
      _      <- SubsystemControlModesFile.write[IO](file, Site.GS, AllFull, "tester")
      first  <- IO.delay(Files.readAllLines(file).asScala.toList.filterNot(_.startsWith("#")))
      _      <- SubsystemControlModesFile.write[IO](file, Site.GS, AllFull, "tester")
      second <- IO.delay(Files.readAllLines(file).asScala.toList.filterNot(_.startsWith("#")))
    yield
      val parsed = ConfigFactory.parseFile(file.toFile)
      assertEquals(parsed.getString("other.key"), "value")
      assertEquals(parsed.getString("observe-engine.odb-queue-polling-interval"), "7 seconds")
      assertEquals(parsed.getString(s"$ConfigPath.tcs"), "full")
      assertEquals(writtenKeys(file), expectedKeys(Site.GS))
      assertEquals(second, first)

  TempDir.test("creates the parent directory and leaves no temporary files"): dir =>
    val file = dir.resolve("conf").resolve("local").resolve("subsystems.conf")
    for
      _ <- SubsystemControlModesFile.write[IO](file, Site.GN, AllFull, "tester")
      _ <- IO.delay(assertEquals(Files.list(file.getParent).count, 1L))
    yield
      Files.deleteIfExists(file)
      Files.deleteIfExists(file.getParent)
      Files.deleteIfExists(file.getParent.getParent)
