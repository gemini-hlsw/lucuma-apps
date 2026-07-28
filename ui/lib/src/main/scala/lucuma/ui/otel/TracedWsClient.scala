// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.otel

import clue.PersistentStreamingClient
import clue.websocket.CloseParams

/**
 * The type of a websocket client wrapped in clue's `Otel4sMiddleware`. The middleware widens the
 * concrete `WebSocketJsClient`, so this is what tracing consumers must hold.
 */
type TracedWsClient[F[_], S] =
  PersistentStreamingClient[F, S, CloseParams, Either[Throwable, CloseParams]]
