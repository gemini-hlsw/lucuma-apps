// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.View
import crystal.react.syntax.all.*
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.DaytimeCalibrationSets
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
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.DaytimeCalibrationSet
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.components.TimeSpanView
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
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
  executionTime:           CalculatedValue[Option[TimeSpan]],
  hasMaterializedSequence: Boolean,
  readonly:                Boolean,
  units:                   WavelengthUnits,
  sequenceChanged:         Callback
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
        def remoteUpdate(d: ObservingMode.DaytimeCalibration): IO[Unit] =
          props.sequenceChanged.toAsync >>
            ctx.odbApi
              .updateObservations(
                List(props.obsId),
                ObservationPropertiesInput(observingMode =
                  ObservingModeInput.DaytimeCalibration(d.toInput).assign
                )
              )

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
                    (old, n) => remoteUpdate(n) >> cb(old, n).toAsync
                  )(f)
              )

        val title: TagMod =
          props.executionTime.value.map: t =>
            <.span("Duration: ", TimeSpanView(t).withMods(props.executionTime.staleClass))
              .withOptionalTooltip(props.executionTime.staleTooltipString.map(s => s: VdomNode))

        val body: TagMod =
          daytimeView.fold(
            if props.modePending then <.div(ExploreStyles.DaytimeCalMessage, "Loading...")
            else EmptyVdom
          ): view =>
            DaytimeCalibrationEditor(view,
                                     props.editable,
                                     props.hasMaterializedSequence,
                                     props.units
            )

        TileContents(title, body)
    })

/** The instrument dropdown and the Daytime Calibration Sets grid. */
case class DaytimeCalibrationEditor(
  daytime:                 View[ObservingMode.DaytimeCalibration],
  editable:                Boolean,
  hasMaterializedSequence: Boolean,
  units:                   WavelengthUnits
) extends ReactFnProps(DaytimeCalibrationEditor.component)

object DaytimeCalibrationEditor:
  private val UnsupportedInstruments: Set[Instrument] =
    Enumerated[Instrument].all.toSet -- DaytimeCalibrationSets.Instruments

  private val component =
    ScalaFnComponent[DaytimeCalibrationEditor]: props =>
      val instrument: Instrument = props.daytime.get.calibrationInstrument

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
        if DaytimeCalibrationSets.Instruments.contains_(instrument) then
          DaytimeCalibrationSetsTable(instrument, setsView, props.editable, props.units)
        else
          <.div(
            ExploreStyles.DaytimeCalMessage,
            "Calibration sets are only supported for GMOS North and GMOS South."
          )

      <.div(
        <.div(
          ExploreStyles.DaytimeCalInstrument,
          <.label("Instrument:", ^.htmlFor := "daytime-cal-instrument"),
          EnumDropdownView(
            id = "daytime-cal-instrument".refined,
            value = instrumentView,
            exclude = UnsupportedInstruments - instrument,
            disabled = !props.editable
          ).withMods(testId     := "explore-daytime-cal-instrument")
        ),
        <.div(
          ExploreStyles.DaytimeCalMessage,
          "The sequence is materialized. Delete it to edit the calibration sets."
        ).when(props.hasMaterializedSequence),
        table
      )
