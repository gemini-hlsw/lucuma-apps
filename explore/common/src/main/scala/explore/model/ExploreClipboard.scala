// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.effect.IO
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalajs.dom
import org.typelevel.log4cats.Logger

object ExploreClipboard:
  private val storageKey = "clipboard"

  def get(using Logger[IO]): IO[LocalClipboard] = IO {
    // getItem returns null when storage isn't set.
    Option(dom.window.localStorage.getItem(storageKey))
      .fold(LocalClipboard.Empty)(decode[LocalClipboard](_).getOrElse(LocalClipboard.Empty))
  }.handleErrorWith(t =>
    Logger[IO].error(t)("Error getting value from localStorage") >> IO(LocalClipboard.Empty)
  )

  def set(item: LocalClipboard)(using Logger[IO]): IO[Unit] = IO {
    item match
      case LocalClipboard.Empty => dom.window.localStorage.removeItem(storageKey)
      case other                => dom.window.localStorage.setItem(storageKey, other.asJson.noSpaces)
  }.handleErrorWith(t => Logger[IO].error(t)("Error setting localStorage"))
