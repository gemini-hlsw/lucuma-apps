// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

/**
 * Messages sent from clients to servers must extend this trait and define the response type.
 */
trait WorkerRequest {
  type ResponseType

  /** Custom span name; when empty, the case-class name (`productPrefix`) is used. */
  def traceName: String = ""
}

object WorkerRequest:
  /**
   * Name of a request, for span naming. A request may override `traceName` for a more specific
   * name; otherwise the case-class name (`productPrefix`) is used. Takes `Any` because servers are
   * generic on their request type, and `productPrefix` avoids reflection, which Scala.js dislikes.
   */
  def name(request: Any): String =
    request match
      case w: WorkerRequest if w.traceName.nonEmpty => w.traceName
      case p: Product                               => p.productPrefix
      case _                                        => "unknown"
