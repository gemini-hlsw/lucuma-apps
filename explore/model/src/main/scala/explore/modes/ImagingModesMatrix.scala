// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Eq
import cats.Order
import cats.derived.*
import cats.implicits.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.SupportedInstruments
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.odb.json.angle.decoder.given
import monocle.Getter
import monocle.Lens
import monocle.macros.GenLens

case class ImagingModeRow(
  id:               Option[Int], // we number the modes for the UI
  instrumentConfig: ItcInstrumentConfig,
  ao:               ModeAO,
  fov:              Angle,
  capability:       Option[ImagingCapability]
) extends ModeRow derives Eq:
  val enabled                        = SupportedInstruments.contains_(instrumentConfig.instrument)
  val filterType: Option[FilterType] =
    instrumentConfig match
      case ItcInstrumentConfig.GmosNorthImaging(filter, _)  => filter.filterType.some
      case ItcInstrumentConfig.GmosSouthImaging(filter, _)  => filter.filterType.some
      case ItcInstrumentConfig.GnirsImaging(filter, _, _)   => filter.filterType.some
      case ItcInstrumentConfig.Flamingos2Imaging(filter, _) => filter.filterType.some
      case _                                                => none

object ImagingModeRow {

  val id: Lens[ImagingModeRow, Option[Int]] = GenLens[ImagingModeRow](_.id)

  val instrumentConfig: Lens[ImagingModeRow, ItcInstrumentConfig] =
    GenLens[ImagingModeRow](_.instrumentConfig)

  val instrument: Getter[ImagingModeRow, Instrument] =
    instrumentConfig.andThen(ItcInstrumentConfig.instrument)

  val ao: Lens[ImagingModeRow, ModeAO] = GenLens[ImagingModeRow](_.ao)

  val fov: Lens[ImagingModeRow, Angle] = GenLens[ImagingModeRow](_.fov)

  val capability: Lens[ImagingModeRow, Option[ImagingCapability]] =
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

  private given Decoder[ItcInstrumentConfig.Flamingos2Imaging] =
    _.downField("filter")
      .as[Flamingos2Filter]
      .map(filter =>
        ItcInstrumentConfig.Flamingos2Imaging(filter, ItcInstrumentConfig.PlaceholderEtm)
      )

  private given Decoder[ItcInstrumentConfig.GnirsImaging] = c =>
    for {
      filter <- c.downField("filter").as[GnirsFilter]
      camera <- c.downField("camera").as[GnirsCamera]
    } yield ItcInstrumentConfig.GnirsImaging(filter, camera, ItcInstrumentConfig.PlaceholderEtm)

  given Decoder[ImagingModeRow] = c =>
    for {
      inst        <- c.downField("instrument").as[Instrument]
      ao          <- c.downField("adaptiveOptics").as[Boolean]
      fov         <- c.downField("fov").as[Angle]
      capability  <- c.downField("capability").as[Option[ImagingCapability]]
      site        <- c.downField("site").as[Site]
      filterLabel <- c.downField("filterLabel").as[NonEmptyString]
      gmosNorth   <- c.downField("gmosNorth").as[Option[ItcInstrumentConfig.GmosNorthImaging]]
      gmosSouth   <- c.downField("gmosSouth").as[Option[ItcInstrumentConfig.GmosSouthImaging]]
      flamingos2  <- c.downField("flamingos2").as[Option[ItcInstrumentConfig.Flamingos2Imaging]]
      gnirs       <- c.downField("gnirs").as[Option[ItcInstrumentConfig.GnirsImaging]]
    } yield {
      val cfg: ItcInstrumentConfig = gmosNorth
        .orElse(gmosSouth)
        .orElse(flamingos2)
        .orElse(gnirs)
        // Alopeke and Zorro only have a label field
        .getOrElse(ItcInstrumentConfig.GenericImaging(inst, filterLabel, site, capability))
      ImagingModeRow(none, cfg, ModeAO(ao), fov, capability)
    }
}

case class ImagingModesMatrix(matrix: List[ImagingModeRow]) derives Eq:
  def filtered(
    minimumFov:  Option[Angle],
    filterTypes: Set[FilterType],
    capability:  Option[ImagingCapability],
    declination: Option[Declination] = None
  ): List[ImagingModeRow] =
    import explore.model.syntax.all.*
    given Order[Angle]                    = Angle.AngleOrder
    val filter: ImagingModeRow => Boolean = r =>
      minimumFov.forall(fov => r.fov >= fov) &&
        (filterTypes.isEmpty || r.filterType.exists(filterTypes.contains)) &&
        capability.forall(r.capability.contains) &&
        declination.forall(r.instrumentConfig.site.inPreferredDeclination)
    matrix.filter(filter)

object ImagingModesMatrix:
  val empty: ImagingModesMatrix = ImagingModesMatrix(Nil)
