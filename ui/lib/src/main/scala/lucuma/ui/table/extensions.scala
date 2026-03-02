// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.table

import crystal.ViewF
import lucuma.react.table.Updater

extension [F[_], A](view: ViewF[F, A])
  def handleTableUpdate: Updater[A] => F[Unit] =
    (u: Updater[A]) =>
      u match
        case Updater.Set(v) => view.set(v)
        case Updater.Mod(f) => view.mod(a => f(a))
