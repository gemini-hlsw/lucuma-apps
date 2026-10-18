// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import io.circe.KeyDecoder
import io.circe.KeyEncoder
import lucuma.core.enums.Instrument
import lucuma.core.util.Enumerated
import observe.model.enums.ObservatorySubsystem

import java.util.UUID

type ObservationName = String
type TargetName      = String

type Subsystem = Instrument | ObservatorySubsystem

val UnknownTargetName = "None"

val CalibrationQueueName: String = "Calibration Queue"
val CalibrationQueueId: QueueId  =
  QueueId(UUID.fromString("7156fa7e-48a6-49d1-a267-dbf3bbaa7577"))

given KeyEncoder[Subsystem] = KeyEncoder.instance:
  case i: Instrument           => i.tag
  case o: ObservatorySubsystem => o.tag

given KeyDecoder[Subsystem] = KeyDecoder.instance: tag =>
  Enumerated[Instrument].fromTag(tag).orElse(Enumerated[ObservatorySubsystem].fromTag(tag))

// Observatory subsystems come before instruments
given Enumerated[Subsystem] = Enumerated
  .from(
    Enumerated[ObservatorySubsystem].all.head,
    (Enumerated[ObservatorySubsystem].all.tail ++ Enumerated[Instrument].all)*
  )
  .withTag:
    case i: Instrument           => i.tag
    case o: ObservatorySubsystem => o.tag
