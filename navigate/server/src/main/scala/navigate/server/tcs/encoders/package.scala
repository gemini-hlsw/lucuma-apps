// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.syntax.all.*
import lucuma.core.enums.GuideProbe
import lucuma.core.util.Enumerated
import mouse.all.*
import navigate.model.enums.CentralBafflePosition
import navigate.model.enums.DeployableBafflePosition
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter
import navigate.model.enums.VirtualTelescope
import navigate.server.acm.Encoder
import navigate.server.acm.Encoder.*

package object encoders {
  given Encoder[GuideProbe, String] = {
    case GuideProbe.GmosOIWFS       => "OIWFS"
    case GuideProbe.Flamingos2OIWFS => "OIWFS"
    case GuideProbe.PWFS1           => "PWFS1"
    case GuideProbe.PWFS2           => "PWFS2"
  }

  given Encoder[CentralBafflePosition, String] = {
    case CentralBafflePosition.Open   => "Open"
    case CentralBafflePosition.Closed => "Closed"
  }

  given Encoder[DeployableBafflePosition, String] = {
    case DeployableBafflePosition.ThermalIR => "Retracted"
    case DeployableBafflePosition.NearIR    => "Near IR"
    case DeployableBafflePosition.Visible   => "Visible"
    case DeployableBafflePosition.Extended  => "Extended"
  }

  given Encoder[ReferenceFrame, String] = (x: ReferenceFrame) =>
    (x match {
      case ReferenceFrame.AzimuthElevation => 0
      case ReferenceFrame.XY               => 1
      case ReferenceFrame.Instrument       => 2
      case ReferenceFrame.Tracking         => 3
    }).toString

  given Encoder[List[VirtualTelescope], String] = (x: List[VirtualTelescope]) => {
    val m: Int = Enumerated[VirtualTelescope].all.zipWithIndex.foldRight(0) {
      case ((vt, idx), acc) => x.contains(vt).fold(acc | (1 << idx), acc)
    }
    (-m).toString
  }

  given Encoder[VirtualTelescope, String] = {
    case VirtualTelescope.SourceA => "SOURCE A"
    case VirtualTelescope.SourceB => "SOURCE B"
    case VirtualTelescope.SourceC => "SOURCE C"
    case VirtualTelescope.Pwfs1   => "PWFS1"
    case VirtualTelescope.Pwfs2   => "PWFS2"
    case VirtualTelescope.Oiwfs   => "OIWFS"
    case a                        => a.tag
  }

  given Encoder[PwfsFilter, String] = {
    case PwfsFilter.Neutral => "neutral"
    case x                  => x.tag
  }

  given Encoder[PwfsFieldStop, String] = {
    case PwfsFieldStop.Prism => "prism"
    case PwfsFieldStop.Fs10  => "10.0"
    case PwfsFieldStop.Fs6_4 => "6.4"
    case PwfsFieldStop.Fs3_2 => "3.2"
    case PwfsFieldStop.Fs1_6 => "1.6"
    case PwfsFieldStop.Open1 => "open1"
    case PwfsFieldStop.Open2 => "open2"
    case PwfsFieldStop.Open3 => "open3"
    case PwfsFieldStop.Open4 => "open4"
  }

  extension (v: String) {
    def decode[A: {Enumerated, Encoder[*, String]}]: Option[A] =
      Enumerated[A].all.find(_.encode === v)
  }

}
