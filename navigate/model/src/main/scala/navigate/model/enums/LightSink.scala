// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model.enums

import cats.Show
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.util.Enumerated
import navigate.model.enums.LightSinkVariant

enum LightSink(val tag: String, val instrument: Instrument, val variant: Option[LightSinkVariant]) derives Enumerated, Show {
  case AcqCamNorth extends LightSink("AcqCamNorth", Instrument.AcqCamNorth, none)
  case AcqCamSouth extends LightSink("AcqCamSouth", Instrument.AcqCamSouth, none)
  case Alopeke extends LightSink("Alopeke", Instrument.Alopeke, none) 
  case Flamingos2 extends LightSink("Flamingos2", Instrument.Flamingos2, none) 
  case Ghost extends LightSink("Ghost", Instrument.Ghost, none) 
  case GmosNorth extends LightSink("GmosNorth", Instrument.GmosNorth, none)
  case GmosSouth extends LightSink("GmosSouth", Instrument.GmosSouth, none)
  case GmosNorthIfu extends LightSink("GmosNorthIfu", Instrument.GmosNorth, LightSinkVariant.GmosIfu.some)
  case GmosSouthIfu extends LightSink("GmosSouthIfu", Instrument.GmosSouth, LightSinkVariant.GmosIfu.some)
  case Gnirs extends LightSink("Gnirs", Instrument.Gnirs, none) 
  case Gpi extends LightSink("Gpi", Instrument.Gpi, none) 
  case Gsaoi extends LightSink("Gsaoi", Instrument.Gsaoi, none) 
  case Igrins2 extends LightSink("Igrins2", Instrument.Igrins2, none) 
  case MaroonX extends LightSink("MaroonX", Instrument.MaroonX, none) 
  case NiriF6 extends LightSink("NiriF6", Instrument.Niri, LightSinkVariant.NiriF6.some)
  case NiriF14 extends LightSink("NiriF14", Instrument.Niri, LightSinkVariant.NiriF14.some)
  case NiriF32 extends LightSink("NiriF32", Instrument.Niri, LightSinkVariant.NiriF32.some)
  case Scorpio extends LightSink("Scorpio", Instrument.Scorpio, none)
  case VisitorNorth extends LightSink("VisitorNorth", Instrument.VisitorNorth, none)
  case VisitorSouth extends LightSink("VisitorSouth", Instrument.VisitorSouth, none)
  case Zorro extends LightSink("Zorro", Instrument.Zorro, none)
}

object LightSink {
  def fromInstrumentAndVariant(instrument: Instrument, variant: Option[LightSinkVariant]): Option[LightSink] = LightSink.values.find(x => x.instrument === instrument && x.variant === variant) 
}
