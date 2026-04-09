// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import io.circe.KeyDecoder
import io.circe.KeyEncoder
import lucuma.core.enums.Instrument
import lucuma.core.util.Enumerated
import observe.model.enums.Resource

import java.util.UUID

type ObservationName = String
type TargetName      = String

enum Server(val tag: String) derives Enumerated:
  case Dhs extends Server("Dhs")

type Subsystem         = Resource | Instrument
type SubsystemOrServer = Subsystem | Server
object SubsystemOrServer:
  def fromSubsystem(s: Subsystem): SubsystemOrServer = s

val UnknownTargetName = "None"

val CalibrationQueueName: String = "Calibration Queue"
val CalibrationQueueId: QueueId  =
  QueueId(UUID.fromString("7156fa7e-48a6-49d1-a267-dbf3bbaa7577"))

given KeyEncoder[Subsystem] = KeyEncoder.instance:
  case r: Resource   => r.tag
  case i: Instrument => i.tag

given KeyDecoder[Subsystem] = KeyDecoder.instance: tag =>
  Enumerated[Resource].fromTag(tag).orElse(Enumerated[Instrument].fromTag(tag))

// Resources come before Instruments
given Enumerated[Subsystem] = Enumerated
  .from(
    Enumerated[Resource].all.head,
    (Enumerated[Resource].all.tail ++ Enumerated[Instrument].all)*
  )
  .withTag:
    case r: Resource   => r.tag
    case i: Instrument => i.tag

given KeyEncoder[SubsystemOrServer] = KeyEncoder.instance:
  case r: Resource   => r.tag
  case i: Instrument => i.tag
  case s: Server     => s.tag

given KeyDecoder[SubsystemOrServer] = KeyDecoder.instance: tag =>
  Enumerated[Resource]
    .fromTag(tag)
    .orElse(Enumerated[Instrument].fromTag(tag))
    .orElse(Enumerated[Server].fromTag(tag))

// Resources come before Instruments before Servers
given Enumerated[SubsystemOrServer] = Enumerated
  .from(
    Enumerated[Subsystem].all.head,
    (Enumerated[Subsystem].all.tail ++ Enumerated[Server].all)*
  )
  .withTag:
    case r: Resource   => r.tag
    case i: Instrument => i.tag
    case s: Server     => s.tag
