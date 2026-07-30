// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

/**
 * Messages sent from clients to servers must extend this trait and define the response type.
 */
trait WorkerRequest {
  type ResponseType
}

object WorkerRequest:
  /**
   * Name of a request, for span naming. Takes `Any` because servers are generic on their request
   * type; `productPrefix` avoids reflection, which Scala.js dislikes.
   */
  def name(request: Any): String =
    request match
      case p: Product => p.productPrefix
      case _          => "unknown"
