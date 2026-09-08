// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

// Huge payloads (e.g. clue dumping a whole GraphQL response) can choke the browser console.
val MaxLogMessageLength: Int = 2000

def truncateLogMessage(msg: String): String =
  if msg.length <= MaxLogMessageLength then msg
  else s"${msg.take(MaxLogMessageLength)}... [truncated ${msg.length - MaxLogMessageLength} chars]"
