// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path
import grackle.SchemaRenderer
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/**
 * Renders the Navigate schema with all its `#import` directives resolved, producing a self
 * contained schema that tools without schema stitching support (e.g. Clue) can read.
 */
object NavigateSchema {
  private given Logger[IO] = NoOpLogger.impl[IO]

  val StitchedSchemaResource: String = "NavigateDB.graphql"

  val render: IO[String] =
    NavigateMappings.loadSchema[IO].map(SchemaRenderer.renderSchema)
}

/**
 * Writes the stitched Navigate schema used by Observe's Clue queries. Run it from the repository
 * root with:
 * {{{
 * sbt "navigate_web_server/Test/runMain navigate.web.server.http4s.RenderNavigateSchema"
 * }}}
 */
object RenderNavigateSchema extends IOApp {
  private val DefaultTarget: String =
    s"observe/server/src/clue/resources/${NavigateSchema.StitchedSchemaResource}"

  def run(args: List[String]): IO[ExitCode] = {
    val target = Path(args.headOption.getOrElse(DefaultTarget))
    NavigateSchema.render
      .flatMap { schema =>
        target.parent.traverse_(Files[IO].createDirectories) >> Stream
          .emit(schema)
          .through(Files[IO].writeUtf8(target))
          .compile
          .drain >> IO.println(
          s"Wrote stitched Navigate schema to $target. Run `sbt observe_server/clueClean` so Clue " +
            "regenerates the Navigate types from it."
        )
      }
      .as(ExitCode.Success)
  }
}
