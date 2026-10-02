// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.tcs

import observe.model.enums.NodAndShuffleStage
import observe.server.ConfigResult
import observe.server.System
import observe.server.tcs.TcsController.*

trait Tcs[F[_]] extends System[F] {
  def nod(stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean): F[ConfigResult[F]]
}
