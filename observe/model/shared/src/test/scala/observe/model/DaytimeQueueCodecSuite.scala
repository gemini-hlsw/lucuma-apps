// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.kernel.laws.discipline.*
import io.circe.testing.CodecTests
import io.circe.testing.instances.*
import lucuma.core.util.arb.ArbEnumerated.given
import observe.model.arb.ArbDaytimeQueue.given
import observe.model.enums.*
import org.scalacheck.Test as ScalaCheckTest

class DaytimeQueueCodecSuite extends munit.DisciplineSuite:
  override def scalaCheckTestParameters: ScalaCheckTest.Parameters =
    ScalaCheckTest.Parameters.default.withMaxSize(10)

  checkAll("Eq[QueueItemStatus]", EqTests[QueueItemStatus].eqv)
  checkAll("Eq[QueueOrderingMode]", EqTests[QueueOrderingMode].eqv)
  checkAll("Eq[QueueDispatchState]", EqTests[QueueDispatchState].eqv)
  checkAll("Eq[QueuePauseId]", EqTests[QueuePauseId].eqv)
  checkAll("Eq[QueueItemId]", EqTests[QueueItemId].eqv)
  checkAll("Codec[QueueItemId]", CodecTests[QueueItemId].unserializableCodec)
  checkAll("Eq[DaytimeQueueItem]", EqTests[DaytimeQueueItem].eqv)
  checkAll("Codec[DaytimeQueueItem]", CodecTests[DaytimeQueueItem].unserializableCodec)
  checkAll("Eq[DaytimeQueue]", EqTests[DaytimeQueue].eqv)
  checkAll("Codec[DaytimeQueue]", CodecTests[DaytimeQueue].unserializableCodec)
  checkAll("Eq[DaytimeQueueView]", EqTests[DaytimeQueueView].eqv)
  checkAll("Codec[DaytimeQueueView]", CodecTests[DaytimeQueueView].unserializableCodec)
