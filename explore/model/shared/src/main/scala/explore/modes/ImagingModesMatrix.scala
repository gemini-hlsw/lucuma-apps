// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Eq
import cats.Order
import cats.derived.*
import cats.implicits.*
import explore.model.SupportedInstruments
import io.circe.Decoder
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.phase0.ImagingCapabilities
import monocle.Getter
import monocle.Lens
import monocle.macros.GenLens
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.refined.given

case class ImagingModeRow(
  id:         Option[Int], // we number the modes for the UI
  instrument: ItcInstrumentConfig,
  ao:         ModeAO,
  fov:        Angle,
  capability: Option[ImagingCapabilities]
) extends ModeRow derives Eq:
  val enabled                        = SupportedInstruments.contains_(instrument.instrument)
  val filterType: Option[FilterType] =
    instrument match
      case ItcInstrumentConfig.GmosNorthImaging(filter, _) => filter.filterType.some
      case ItcInstrumentConfig.GmosSouthImaging(filter, _) => filter.filterType.some
      case _                                               => none

object ImagingModeRow {

  val id: Lens[ImagingModeRow, Option[Int]] = GenLens[ImagingModeRow](_.id)

  val instrumentConfig: Lens[ImagingModeRow, ItcInstrumentConfig] =
    GenLens[ImagingModeRow](_.instrument)

  val instrument: Getter[ImagingModeRow, Instrument] =
    instrumentConfig.andThen(ItcInstrumentConfig.instrument)

  val ao: Lens[ImagingModeRow, ModeAO] = GenLens[ImagingModeRow](_.ao)

  val fov: Lens[ImagingModeRow, Angle] = GenLens[ImagingModeRow](_.fov)

  val capability: Lens[ImagingModeRow, Option[ImagingCapabilities]] =
    GenLens[ImagingModeRow](_.capability)

  val filter: Getter[ImagingModeRow, ItcInstrumentConfig#Filter] =
    instrumentConfig.andThen(ItcInstrumentConfig.filter)

  // decoders for instruments are used locally as they are not lawful
  private given Decoder[ItcInstrumentConfig.GmosNorthImaging] =
    _.downField("filter")
      .as[GmosNorthFilter]
      .map(filter =>
        ItcInstrumentConfig.GmosNorthImaging(filter, ItcInstrumentConfig.PlaceholderEtm)
      )

  private given Decoder[ItcInstrumentConfig.GmosSouthImaging] =
    _.downField("filter")
      .as[GmosSouthFilter]
      .map(filter =>
        ItcInstrumentConfig.GmosSouthImaging(filter, ItcInstrumentConfig.PlaceholderEtm)
      )

  given Decoder[ImagingModeRow] = c =>
    for {
      inst        <- c.downField("instrument").as[Instrument]
      ao          <- c.downField("adaptiveOptics").as[Boolean]
      fov         <- c.downField("fov").as[Angle]
      capability  <- c.downField("capability").as[Option[ImagingCapabilities]]
      site        <- c.downField("site").as[Site]
      filterLabel <- c.downField("filterLabel").as[NonEmptyString]
      gmosNorth   <- c.downField("gmosNorth").as[Option[ItcInstrumentConfig.GmosNorthImaging]]
      gmosSouth   <- c.downField("gmosSouth").as[Option[ItcInstrumentConfig.GmosSouthImaging]]
    } yield {
      val cfg: ItcInstrumentConfig = gmosNorth
        .orElse(gmosSouth)
        // Alopeke and Zorro only have a label field
        .getOrElse(ItcInstrumentConfig.GenericImaging(inst, filterLabel, site, capability))
      ImagingModeRow(none, cfg, ModeAO(ao), fov, capability)
    }
}

case class ImagingModesMatrix(matrix: List[ImagingModeRow]) derives Eq:
  def filtered(
    minimumFov:  Option[Angle],
    filterTypes: Set[FilterType],
    declination: Option[Declination] = None
  ): List[ImagingModeRow] =
    import explore.model.syntax.all.*
    given Order[Angle]                    = Angle.AngleOrder
    val filter: ImagingModeRow => Boolean = r =>
      minimumFov.forall(fov => r.fov >= fov) &&
        (filterTypes.isEmpty || r.filterType.exists(filterTypes.contains)) &&
        declination.forall(r.instrument.site.inPreferredDeclination)

    matrix.filter(filter)

object ImagingModesMatrix {
  val empty: ImagingModesMatrix = ImagingModesMatrix(Nil)
}
