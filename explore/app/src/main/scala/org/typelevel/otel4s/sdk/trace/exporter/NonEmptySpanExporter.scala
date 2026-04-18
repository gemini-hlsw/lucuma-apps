// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.typelevel.otel4s.sdk.trace.exporter

import cats.Applicative
import cats.Foldable
import cats.syntax.all.*
import org.typelevel.otel4s.sdk.trace.data.SpanData

class NonEmptySpanExporter[F[_]: Applicative](underlying: SpanExporter[F])
    extends SpanExporter.Unsealed[F]:
  val name: String = s"NonEmpty(${underlying.name})"
  def exportSpans[G[_]: Foldable](spans: G[SpanData]): F[Unit] =
    if spans.isEmpty then Applicative[F].unit
    else underlying.exportSpans(spans)
  def flush: F[Unit] = underlying.flush
