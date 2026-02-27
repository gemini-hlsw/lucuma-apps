// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.table

import cats.Applicative
import crystal.ViewF
import lucuma.react.table.Updater

import scalajs.js

extension [F[_]: Applicative, A](view: ViewF[F, A])
  def handleTableUpdate: Updater[A] => F[Unit] =
    (u: Updater[A]) =>
      u match
        case Updater.Set(v) =>
          println(s"Setting table state to $v")
          v.asInstanceOf[js.UndefOr[A]].map(view.set(_)).getOrElse(Applicative[F].unit)
        case Updater.Mod(f) =>
          // Applicative[F].pure(println("Mod suppressed"))
          view.mod(a =>
            val b = f(a).asInstanceOf[js.UndefOr[A]].getOrElse(a)
            println(s"Updating table state from $a to $b")
            b
          )
