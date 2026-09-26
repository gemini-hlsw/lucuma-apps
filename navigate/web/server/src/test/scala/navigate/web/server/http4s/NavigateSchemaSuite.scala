// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s

import cats.effect.IO
import cats.syntax.all.*
import munit.CatsEffectSuite

import scala.io.Source

/**
 * Checks that the stitched copy of the Navigate schema used by Navigate's clients is in sync with
 * `navigate.graphql`.
 */
class NavigateSchemaSuite extends CatsEffectSuite {

  private val stitchedCopy: IO[String] =
    IO.blocking {
      val source = Source.fromResource(NavigateSchema.StitchedSchemaResource)
      try source.mkString
      finally source.close()
    }

  test("Observe's copy of the stitched Navigate schema is up to date") {
    (NavigateSchema.render, stitchedCopy).flatMapN { (expected, obtained) =>
      IO(
        assertEquals(
          obtained,
          expected,
          s"${NavigateSchema.StitchedSchemaResource} is out of date, regenerate it with: " +
            "sbt navigateSchemaGenerate"
        )
      )
    }
  }
}
