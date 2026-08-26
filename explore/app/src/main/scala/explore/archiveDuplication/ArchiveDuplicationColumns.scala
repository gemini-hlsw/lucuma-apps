// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.archiveDuplication

import cats.Eq
import cats.Order.given
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import explore.Icons
import explore.model.AppContext
import explore.model.ArchiveMatch
import explore.model.MatchCountCell
import explore.model.Observation
import explore.model.display
import explore.model.enums.WavelengthUnits
import explore.model.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.time.format.GppDateFormatter
import lucuma.react.primereact.Button
import lucuma.react.primereact.tooltip.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.format.*
import lucuma.ui.primereact.*
import lucuma.ui.table.*

import scala.collection.immutable.TreeSeqMap

/** Whether the Search controls are live, and why not when they are not. */
case class ArchiveDuplicationControls(enabled: Boolean, disabledReason: Option[String]) derives Eq

/**
 * One semantic column set for both row kinds: a column means the same kind of thing on an
 * observation row as on an Archive Match row, filled from whichever source that row has.
 *
 * That is what makes the comparison read straight down the column.
 */
object ArchiveDuplicationColumns:
  import ArchiveDuplicationRow.*

  private type Row = Expandable[ArchiveDuplicationRow]

  private val ColDef = ColumnDef[Row].WithColumnFilters.WithGlobalFilter[String]

  val ExpanderColumnId: ColumnId             = ColumnId("expander")
  val ObservationIdColumnId: ColumnId        = ColumnId("observation_id")
  val MatchCountColumnId: ColumnId           = ColumnId("match_count")
  val TargetColumnId: ColumnId               = ColumnId("target")
  val RAColumnId: ColumnId                   = ColumnId("ra")
  val DecColumnId: ColumnId                  = ColumnId("dec")
  val InstrumentColumnId: ColumnId           = ColumnId("instrument")
  val DisperserColumnId: ColumnId            = ColumnId("disperser")
  val WavelengthColumnId: ColumnId           = ColumnId("wavelength")
  val FilterColumnId: ColumnId               = ColumnId("filter")
  val ObservationDateColumnId: ColumnId      = ColumnId("observation_date")
  val ReleaseDateColumnId: ColumnId          = ColumnId("release_date")
  val LastCheckedColumnId: ColumnId          = ColumnId("last_checked")
  val ExposureColumnId: ColumnId             = ColumnId("exposure")
  val DataLabelColumnId: ColumnId            = ColumnId("data_label")
  val DistanceColumnId: ColumnId             = ColumnId("distance")
  val ProgramReferenceColumnId: ColumnId     = ColumnId("archive_program")
  val ObservationReferenceColumnId: ColumnId = ColumnId("archive_observation")
  val QaStateColumnId: ColumnId              = ColumnId("qa_state")
  val FileNameColumnId: ColumnId             = ColumnId("file_name")
  val ActionsColumnId: ColumnId              = ColumnId("actions")

  private val ColumnNames: TreeSeqMap[ColumnId, String] =
    TreeSeqMap(
      ExpanderColumnId             -> " ",
      ObservationIdColumnId        -> "Observation",
      MatchCountColumnId           -> "Matches",
      TargetColumnId               -> "Target",
      RAColumnId                   -> "RA",
      DecColumnId                  -> "Dec",
      InstrumentColumnId           -> "Instrument",
      DisperserColumnId            -> "Disperser",
      WavelengthColumnId           -> "λ",
      FilterColumnId               -> "Filter",
      ObservationDateColumnId      -> "Date",
      ReleaseDateColumnId          -> "Release",
      LastCheckedColumnId          -> "Checked",
      ExposureColumnId             -> "Exposure",
      DataLabelColumnId            -> "Data Label",
      DistanceColumnId             -> "Distance",
      ProgramReferenceColumnId     -> "Archive Program",
      ObservationReferenceColumnId -> "Archive Observation",
      QaStateColumnId              -> "QA",
      FileNameColumnId             -> "File",
      ActionsColumnId              -> "  "
    )

  val ColumnsExcludedFromVisibility: Set[ColumnId] =
    Set(ExpanderColumnId, ActionsColumnId)

  val SelectableColumnNames: List[(ColumnId, String)] =
    ColumnNames.filterNot((k, _) => ColumnsExcludedFromVisibility.contains(k)).toList

  val DefaultColumnVisibility: ColumnVisibility =
    ColumnVisibility(
      ExposureColumnId             -> Visibility.Hidden,
      DataLabelColumnId            -> Visibility.Hidden,
      DistanceColumnId             -> Visibility.Hidden,
      ProgramReferenceColumnId     -> Visibility.Hidden,
      ObservationReferenceColumnId -> Visibility.Hidden,
      QaStateColumnId              -> Visibility.Hidden,
      FileNameColumnId             -> Visibility.Hidden
    )

  private def instrumentOf(modeType: ObservingModeType): Option[Instrument] =
    modeType.fold(_ => none, _.instrument.some, _.instrument.some)

  // The dispersing element in the beam, as the observation's own configuration states it.
  private def disperserOf(config: BasicConfiguration): Option[String] =
    config match
      case BasicConfiguration.GmosNorthLongSlit(grating = g)    => g.shortName.some
      case BasicConfiguration.GmosSouthLongSlit(grating = g)    => g.shortName.some
      case BasicConfiguration.GmosNorthMos(grating = g)         => g.shortName.some
      case BasicConfiguration.GmosSouthMos(grating = g)         => g.shortName.some
      case BasicConfiguration.Flamingos2LongSlit(disperser = d) => d.shortName.some
      case BasicConfiguration.Flamingos2Mos(disperser = d)      => d.shortName.some
      case BasicConfiguration.GnirsSpectroscopy(grating = g)    => g.shortName.some
      case _                                                    => none

  private def filterOf(config: BasicConfiguration): Option[String] =
    config match
      case BasicConfiguration.GmosNorthLongSlit(filter = f)  => f.map(_.shortName)
      case BasicConfiguration.GmosSouthLongSlit(filter = f)  => f.map(_.shortName)
      case BasicConfiguration.GmosNorthMos(filter = f)       => f.map(_.shortName)
      case BasicConfiguration.GmosSouthMos(filter = f)       => f.map(_.shortName)
      case BasicConfiguration.Flamingos2LongSlit(filter = f) => f.shortName.some
      case BasicConfiguration.Flamingos2Mos(filter = f)      => f.shortName.some
      case BasicConfiguration.GnirsSpectroscopy(filter = f)  => f.shortName.some
      case BasicConfiguration.GmosNorthImaging(filters)      =>
        filters.map(_.shortName).toList.mkString(", ").some
      case BasicConfiguration.GmosSouthImaging(filters)      =>
        filters.map(_.shortName).toList.mkString(", ").some
      case BasicConfiguration.Flamingos2Imaging(filters)     =>
        filters.map(_.shortName).toList.mkString(", ").some
      case BasicConfiguration.GnirsImaging(filters = fs)     =>
        fs.map(_.shortName).toList.mkString(", ").some
      case _                                                 => none

  private def formatWv(w: Wavelength): String =
    s"${display.wavelengthDisplay(WavelengthUnits.Nanometers).shortName(w)} nm"

  private def formatDistance(a: Angle): String =
    f"${Angle.signedDecimalArcseconds.get(a).toDouble}%.1f\""

  // The ODB computes staleness against the observation as it now stands
  private val StaleTooltip =
    "The observation has changed since this search ran. Re-check the row to bring it up to date."

  private def matchCountText(count: Int, saturated: Boolean): String =
    if saturated then s"$count+" else count.toString

  // State-aware rather than a plain number
  private def matchCountCell(cell: MatchCountCell): VdomNode =
    cell match
      case MatchCountCell.Loading                                  =>
        <.span(Icons.Spinner.withSpin(true))
          .withTooltip(content = "Loading the stored Search results…")
      case MatchCountCell.Searching                                =>
        <.span(Icons.ArrowRotateRight.withSpin(true))
          .withTooltip(content = "Searching the archive…")
      case MatchCountCell.NotChecked                               =>
        <.span(Icons.CircleQuestion)
          .withTooltip(content = "Not checked yet. Run the Search with the row's re-check button.")
      case MatchCountCell.NotApplicable                            =>
        <.span("n/a")
      case MatchCountCell.Counted(count, saturated, stale)         =>
        if stale then
          <.span(matchCountText(count.value, saturated), " ", Icons.ClockRotateLeft)
            .withTooltip(content = StaleTooltip)
        else <.span(matchCountText(count.value, saturated))
      case MatchCountCell.SearchFailed(count, saturated, e, stale) =>
        <.span(matchCountText(count.value, saturated), " ", Icons.ExclamationTriangle)
          .withTooltip(content =
            s"The last Search failed, so these matches may be out of date. ${e.foldMap(_.value)}" +
              (if stale then s" $StaleTooltip" else "")
          )
      case MatchCountCell.CallFailed(message)                      =>
        <.span(Icons.ExclamationTriangle).withTooltip(content = message)

  def columns(
    programId: Program.Id,
    ctx:       AppContext[IO],
    controls:  ArchiveDuplicationControls,
    onExpand:  Observation.Id => Callback,
    onRecheck: Observation.Id => Callback
  ) =
    def col[V](id: ColumnId, accessor: ArchiveDuplicationRow => V): ColDef.TypeFor[V] =
      ColDef(id, r => accessor(r.value), ColumnNames(id))

    def textCol(
      id:       ColumnId,
      forObs:   ObsRow => Option[String],
      forMatch: ArchiveMatch => Option[String]
    ) =
      col(id, _.fold(forObs, r => forMatch(r.archiveMatch), _ => none))
        .withFilterMethod(FilterMethod.Text(_.orEmpty))
        .withCell(_.value.orEmpty)
        .sortable

    // Filtering acts on observation rows
    def matchOnlyTextCol(id: ColumnId, forMatch: ArchiveMatch => Option[String]) =
      col(id, _.optMatch.flatMap(forMatch))
        .withCell(_.value.orEmpty)
        .sortable

    List(
      ColDef(
        ExpanderColumnId,
        cell = cell =>
          if cell.row.original.value.isObsRow && cell.row.getCanExpand() then
            <.span(
              ^.cursor.pointer,
              TableStyles.ExpanderChevron,
              TableStyles.ExpanderChevronOpen.when(cell.row.getIsExpanded()),
              ^.onClick ==> (e =>
                e.stopPropagationCB *>
                  onExpand(cell.row.original.value.observationId)
                    .unless_(cell.row.getIsExpanded()) *>
                  cell.row.toggleExpanded()
              )
            )(TableIcons.ChevronRight.withFixedWidth(true))
          else "",
        enableResizing = false
      ).withSize(35.toPx),
      col(ObservationIdColumnId, _.optEntry.map(_.observation))
        .withFilterMethod:
          FilterMethod.Text(_.foldMap(o => o.reference.fold(o.id.show)(_.label)))
        .withCell: cell =>
          cell.value.map: obs =>
            ctx.obsIdRoutingLink(
              programId,
              obs.id,
              contents = obs.reference.map(r => <.span(r.label): VdomNode)
            )
        .sortableBy(_.map(_.id)),
      col(MatchCountColumnId, _.optEntry.map(_.matchCount))
        .withCell: cell =>
          cell.row.original.value.optEntry.map(e => matchCountCell(e.matchCountCell))
        .sortable
        .withSize(70.toPx),
      textCol(TargetColumnId, r => r.entry.observation.title.some, _.objectName),
      col(
        RAColumnId,
        _.fold(_.entry.basePosition.map(_.ra), _.archiveMatch.coordinates.map(_.ra), _ => none)
      ).withFilterMethod(FilterMethod.Text(_.foldMap(MathValidators.truncatedRA.reverseGet)))
        .withCell(_.value.map(MathValidators.truncatedRA.reverseGet).orEmpty)
        .sortable,
      col(
        DecColumnId,
        _.fold(_.entry.basePosition.map(_.dec), _.archiveMatch.coordinates.map(_.dec), _ => none)
      ).withFilterMethod(FilterMethod.Text(_.foldMap(MathValidators.truncatedDec.reverseGet)))
        .withCell(_.value.map(MathValidators.truncatedDec.reverseGet).orEmpty)
        .sortable,
      textCol(
        InstrumentColumnId,
        _.entry.observation.basicConfiguration
          .map(_.obsModeType)
          .flatMap(instrumentOf)
          .map(_.shortName),
        m => m.instrument.map(_.shortName).orElse(m.instrumentString.some)
      ),
      textCol(
        DisperserColumnId,
        _.entry.observation.basicConfiguration.flatMap(disperserOf),
        _.disperser
      ),
      col(
        WavelengthColumnId,
        _.fold(
          _.entry.observation.basicConfiguration.flatMap(_.centralWv).map(_.value),
          _.archiveMatch.wavelength,
          _ => none
        )
      ).withFilterMethod(FilterMethod.Text(_.foldMap(formatWv)))
        .withCell(_.value.map(formatWv).orEmpty)
        .sortable,
      textCol(FilterColumnId, _.entry.observation.basicConfiguration.flatMap(filterOf), _.filter),
      col(
        ObservationDateColumnId,
        _.fold(
          _.entry.observation.observationTime.flatMap(Timestamp.fromInstantTruncated),
          _.archiveMatch.utDateTime,
          _ => none
        )
      ).withFilterMethod(FilterMethod.Text(_.foldMap(_.formatUtc)))
        .withCell(_.value.map(_.formatUtc).orEmpty)
        .sortable,
      col(ReleaseDateColumnId, _.optMatch.flatMap(_.releaseDate))
        .withCell(_.value.map(GppDateFormatter.format).orEmpty)
        .sortable,
      col(LastCheckedColumnId, _.optEntry.flatMap(_.duplication.toOption).flatMap(_.lastCheckedAt))
        .withFilterMethod(FilterMethod.Text(_.foldMap(_.formatUtc)))
        .withCell(_.value.map(_.formatUtc).orEmpty)
        .sortable,
      col(
        ExposureColumnId,
        // The observation's own per-exposure time, not its whole program time estimate: the
        // column has to mean the same kind of thing as a matched file's exposure.
        _.fold(
          _.entry.observation.scienceRequirements.exposureTimeMode
            .flatMap(ExposureTimeMode.timeAndCount.getOption)
            .map(_.time),
          _.archiveMatch.exposure,
          _ => none[TimeSpan]
        )
      ).withFilterMethod(FilterMethod.Text(_.foldMap(_.formatSeconds)))
        .withCell(_.value.map(_.formatSeconds).orEmpty)
        .sortable,
      matchOnlyTextCol(DataLabelColumnId, _.dataLabel),
      col(DistanceColumnId, _.optMatch.flatMap(_.distance))
        .withCell(_.value.map(formatDistance).orEmpty)
        .sortableBy(_.map(_.toMicroarcseconds)),
      textCol(
        ProgramReferenceColumnId,
        _.entry.observation.reference.map(_.programReference.label),
        _.programReference
      ),
      textCol(
        ObservationReferenceColumnId,
        _.entry.observation.reference.map(_.label),
        _.observationReference
      ),
      matchOnlyTextCol(QaStateColumnId, _.qaStateString),
      matchOnlyTextCol(FileNameColumnId, _.name.some),
      ColDef(
        ActionsColumnId,
        cell = cell =>
          cell.row.original.value.optEntry.map: entry =>
            Button(
              icon = Icons.ArrowRotateRight,
              text = true,
              disabled = !controls.enabled || entry.duplication.isPending,
              tooltip = controls.disabledReason.getOrElse("refresh"),
              onClick = onRecheck(entry.id)
            ).tiny.compact,
        header = ColumnNames(ActionsColumnId)
      ).withSize(45.toPx).setEnableSorting(false.some)
    )
  end columns
end ArchiveDuplicationColumns
