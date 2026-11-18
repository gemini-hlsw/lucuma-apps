// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import crystal.react.syntax.all.*
import explore.Icons
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.DaytimeCalibrationSets
import explore.model.MosMask
import explore.model.MosMasks
import explore.model.ObsTabTileIds
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.util.CalculatedValue
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.refined.*
import lucuma.schemas.model.DaytimeCalibrationSet
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.undo.*
import monocle.Optional
import monocle.std.option.some

/**
 * The Daytime Calibration tile: the instrument of the calibration and the ordered Daytime
 * Calibration Sets its sequence is generated from.
 */
final case class DaytimeConfigurationTile(
  obsId:                   Observation.Id,
  mode:                    UndoSetter[Option[ObservingMode]],
  modePending:             Boolean,
  pendingInstrument:       Option[Instrument],
  executionTime:           CalculatedValue[Option[TimeSpan]],
  hasMaterializedSequence: Boolean,
  readonly:                Boolean,
  isStaff:                 Boolean,
  mosMasks:                View[MosMasks],
  units:                   WavelengthUnits,
  sequenceChanged:         Callback,
  sequenceUnchanged:       Callback
) extends Tile[DaytimeConfigurationTile](
      ObsTabTileIds.DaytimeConfigurationId.id,
      "Daytime Calibration",
      autoHeight = true
    )(DaytimeConfigurationTile):
  val editable: Boolean = !readonly && !hasMaterializedSequence

object DaytimeConfigurationTile
    extends TileComponent[DaytimeConfigurationTile]({ (props, _) =>
      val daytimeOptional: Optional[Option[ObservingMode], ObservingMode.DaytimeCalibration] =
        some[ObservingMode].andThen(ObservingMode.daytimeCalibration)

      for ctx <- useContext(AppContext.ctx)
      yield
        // Every edit replaces the whole mode in the ODB, which also replaces the set list.  The
        // sequence is marked as changed before the mutation: the edit event it produces refreshes
        // the sequence, and the tile only leaves its spinner on a refresh that follows the mark.
        // A rejected edit, such as an unusable mask, is reported and undone locally; no edit event
        // follows, so the mark is cleared.
        def remoteUpdate(
          old: ObservingMode.DaytimeCalibration,
          d:   ObservingMode.DaytimeCalibration
        ): IO[Unit] =
          (props.sequenceChanged.toAsync >>
            ctx.odbApi.updateDaytimeCalibration(props.obsId, d.toInput))
            .handleErrorWith: e =>
              ctx.toastCtx.showToast(e.getMessage, Message.Severity.Error, true) >>
                (props.mode.model.zoom(daytimeOptional).set(old) >>
                  props.sequenceUnchanged).toAsync

        val daytimeView: Option[View[ObservingMode.DaytimeCalibration]] =
          props.mode
            .zoom(daytimeOptional)
            .map: undo =>
              View[ObservingMode.DaytimeCalibration](
                undo.get,
                (f, cb) =>
                  undo.mod[ObservingMode.DaytimeCalibration](
                    identity,
                    n => _ => n,
                    (old, n) => remoteUpdate(old, n) >> cb(old, n).toAsync
                  )(f)
              )

        val title: TagMod =
          props.executionTime.value.map: t =>
            <.span(ExploreStyles.SequenceTileTitle)(
              <.span(ExploreStyles.SequenceTileTitleSummary)(
                timeDisplay(
                  "Duration",
                  t,
                  timeClass = props.executionTime.staleClass,
                  timeTooltip = props.executionTime.staleTooltipString.map(s => s: VdomNode)
                )
              )
            )

        // Until the sets are hydrated, show the editor read-only and empty for the instrument
        // the summary already knows.
        val placeholder: TagMod =
          props.pendingInstrument
            .filter(_ => props.modePending)
            .fold(EmptyVdom): i =>
              DaytimeCalibrationEditor(
                View(ObservingMode.DaytimeCalibration(i, Nil), (_, _) => Callback.empty),
                editable = false,
                hasMaterializedSequence = props.hasMaterializedSequence,
                isStaff = props.isStaff,
                mosMasks = props.mosMasks,
                units = props.units,
                loading = true
              )

        val body: TagMod =
          daytimeView.fold(placeholder): view =>
            DaytimeCalibrationEditor(
              view,
              props.editable,
              props.hasMaterializedSequence,
              props.isStaff,
              props.mosMasks,
              props.units
            )

        TileContents(title, body, heightSettled = !props.modePending)
    })

/** The instrument dropdown and the Daytime Calibration Sets grid. */
case class DaytimeCalibrationEditor(
  daytime:                 View[ObservingMode.DaytimeCalibration],
  editable:                Boolean,
  hasMaterializedSequence: Boolean,
  isStaff:                 Boolean,
  mosMasks:                View[MosMasks],
  units:                   WavelengthUnits,
  loading:                 Boolean = false
) extends ReactFnProps(DaytimeCalibrationEditor.component)

object DaytimeCalibrationEditor:
  private val UnsupportedInstruments: Set[Instrument] =
    Enumerated[Instrument].all.toSet -- DaytimeCalibrationSets.Instruments

  private val component =
    ScalaFnComponent[DaytimeCalibrationEditor]: props =>
      val instrument: Instrument = props.daytime.get.calibrationInstrument
      val offersMasks: Boolean   =
        props.isStaff && DaytimeCalibrationSets.MaskInstruments.contains(instrument)

      val masksView: View[Pot[List[MosMask]]] =
        props.mosMasks.zoom(_.getOrElse(instrument, Pot.pending)): f =>
          m => m.updated(instrument, f(m.getOrElse(instrument, Pot.pending)))

      for
        ctx <- useContext(AppContext.ctx)
        _   <- useEffectWithDeps((instrument, offersMasks)): (instrument, offersMasks) =>
                 import ctx.given

                 val load: IO[Unit] =
                   ctx.odbApi
                     .mosMasks(instrument)
                     .attempt
                     .flatMap: r =>
                       masksView.async.set(r.fold(Pot.Error(_), Pot.Ready(_)))
                 (masksView.set(Pot.pending) >> load.runAsync)
                   .when_(offersMasks && !props.mosMasks.get.contains(instrument))
      yield
        import ctx.given

        def reload: Callback =
          masksView.set(Pot.pending) >>
            ctx.odbApi
              .mosMasks(instrument)
              .attempt
              .flatMap(r => masksView.async.set(r.fold(Pot.Error(_), Pot.Ready(_))))
              .runAsync

        // Switching instrument clears the rows, as one undo step with the instrument.
        val instrumentView: View[Instrument] =
          props.daytime.zoom(_.calibrationInstrument): f =>
            d =>
              val i = f(d.calibrationInstrument)
              if i === d.calibrationInstrument then d
              else ObservingMode.DaytimeCalibration(i, Nil)

        val setsView: View[List[DaytimeCalibrationSet]] =
          props.daytime.zoom(ObservingMode.DaytimeCalibration.calibrationSets)

        val table: VdomNode =
          if DaytimeCalibrationSets.supports(instrument) then
            DaytimeCalibrationSetsTable(
              instrument,
              setsView,
              props.editable,
              props.isStaff,
              Option.when(offersMasks)(masksView.get),
              props.units,
              props.loading
            )
          else
            <.div(
              ExploreStyles.DaytimeCalMessage,
              s"Calibration sets are only supported for ${DaytimeCalibrationSets.Instruments
                  .map(_.longName)
                  .mkString(", ")}."
            )

        val masksStatus: VdomNode =
          masksView.get match
            case Pot.Ready(l) => s"${l.distinctBy(_.name).length} masks"
            case Pot.Pending  => "Loading masks..."
            case Pot.Error(e) => s"Masks could not be loaded: ${e.getMessage}"

        <.div(
          <.div(
            ExploreStyles.DaytimeCalInstrument,
            <.label("Instrument:", ^.htmlFor := "daytime-cal-instrument"),
            EnumDropdownView(
              id = "daytime-cal-instrument".refined,
              value = instrumentView,
              exclude = UnsupportedInstruments - instrument,
              disabled = !props.editable
            ).withMods(testId     := "explore-daytime-cal-instrument"),
            React
              .Fragment(
                <.span(ExploreStyles.DaytimeCalMessage, masksStatus),
                Button(
                  icon = Icons.ArrowsRotate,
                  text = true,
                  tooltip = "Reload the masks of every program",
                  disabled = masksView.get.isPending,
                  onClick = reload
                ).tiny.compact
              )
              .when(offersMasks)
          ),
          <.div(
            ExploreStyles.DaytimeCalMessage,
            "The sequence is materialized, calibration sets can no longer be edited."
          ).when(props.hasMaterializedSequence),
          table
        )
