// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.config

import cats.effect.Sync
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigObject
import com.typesafe.config.ConfigRenderOptions
import com.typesafe.config.ConfigValueFactory
import lucuma.core.enums.Site
import observe.model.config.SubsystemControlModes
import pureconfig.ConfigWriter

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.nio.file.attribute.PosixFilePermissions
import java.time.Instant
import scala.jdk.CollectionConverters.*
import scala.util.Try

/**
 * The `subsystems.conf` file that overrides the `observe-engine.subsystem-control-modes`
 * configuration. It is read at startup (see `WebServerLauncher`) and written by the Observe UI.
 */
object SubsystemControlModesFile:
  private val ConfigPath: String = "observe-engine.subsystem-control-modes"

  /** Location of the file, `<base>/conf/local/subsystems.conf`. */
  def path(base: Path): Path =
    base.resolve("conf").resolve("local").resolve("subsystems.conf")

  private def header(timestamp: Instant, user: String): String =
    s"""# Written by Observe on $timestamp by $user.
       |# Edit through the Observe UI (Subsystems dialog).
       |""".stripMargin

  /**
   * Writes the modes of the subsystems of `site`, all of them explicitly, simulated ones included.
   * Other keys already in the file survive; the other site's subsystems are not written and fall
   * back to `base.conf`. The file is replaced atomically.
   */
  def write[F[_]: Sync](
    path:   Path,
    site:   Site,
    config: SubsystemControlModes,
    user:   String
  ): F[Unit] =
    Sync[F].blocking:
      val written: ConfigObject  =
        ConfigWriter[SubsystemControlModes].to(config) match
          case o: ConfigObject => o
          case other           => throw new IllegalStateException(s"Unexpected config value $other")
      val siteKeys               = SubsystemControlModes.fieldsFor(site).map(_.configKey)
      val filtered: ConfigObject =
        ConfigValueFactory.fromMap(
          siteKeys.map(k => k -> written.get(k).unwrapped).toMap.asJava
        )
      val base                   =
        if Files.exists(path) then ConfigFactory.parseFile(path.toFile) else ConfigFactory.empty
      val rendered               =
        base
          .withValue(ConfigPath, filtered)
          .root
          .render(
            ConfigRenderOptions.defaults.setOriginComments(false).setComments(false).setJson(false)
          )
      val content                = header(Instant.now, user) + rendered
      val dir                    = Option(path.getParent).getOrElse(Path.of("."))
      // Written directly rather than with pureconfig's `saveConfigAsPropertyFileF`: up to 0.17.11 it
      // leaves trailing NUL bytes in the file, which then fails to parse. Fixed by
      // https://github.com/pureconfig/pureconfig/pull/2018; we can switch once it is released.
      Files.createDirectories(dir)
      val tmp                    = Files.createTempFile(dir, "subsystems", ".conf.tmp")
      try
        Files.writeString(tmp, content)
        Try(Files.setPosixFilePermissions(tmp, PosixFilePermissions.fromString("rw-r--r--")))
        val _ = Files.move(
          tmp,
          path,
          StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING
        )
      finally
        val _ = Files.deleteIfExists(tmp)
