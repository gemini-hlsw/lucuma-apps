// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// This lives under `org.typelevel.otel4s` because `SpanExporter` is sealed and its only extension
// point, `SpanExporter.Unsealed`, is `private[otel4s]`.
package org.typelevel.otel4s.sdk.trace.exporter

import cats.Applicative
import cats.Foldable
import cats.syntax.all.*
import org.typelevel.otel4s.sdk.trace.data.SpanData

/**
 * Drops empty batches instead of forwarding them.
 *
 * `BatchSpanProcessor` calls the exporter on every schedule tick regardless of batch size, and
 * `OtlpSpanExporter` issues a request regardless of batch contents. Together that means a CORS
 * preflight plus a POST per tick, per page and per worker, for as long as they are idle. Remove
 * this once otel4s guards against empty batches itself.
 */
private class NonEmptySpanExporter[F[_]: Applicative](underlying: SpanExporter[F])
    extends SpanExporter.Unsealed[F]:

  val name: String = s"NonEmptySpanExporter{${underlying.name}}"

  def exportSpans[G[_]: Foldable](spans: G[SpanData]): F[Unit] =
    if spans.isEmpty then Applicative[F].unit else underlying.exportSpans(spans)

  def flush: F[Unit] = underlying.flush

object NonEmptySpanExporter:
  def apply[F[_]: Applicative](underlying: SpanExporter[F]): SpanExporter[F] =
    new NonEmptySpanExporter(underlying)
