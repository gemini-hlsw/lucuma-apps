// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Order.given
import cats.data.EitherNec
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import crystal.react.*
import crystal.react.hooks.*
import explore.*
import explore.common.Aligner
import explore.common.ScienceQueries.ScienceRequirementsUndoView
import explore.common.ScienceQueries.UpdateScienceRequirements
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.IsActive
import explore.model.ObsConfiguration
import explore.model.ObsIdSetEditInfo
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.ObservingModeGroupList
import explore.model.ObservingModeSummary
import explore.model.PosAngleConstraintAndObsMode
import explore.model.ScienceRequirements
import explore.model.TargetList
import explore.model.enums.PosAngleOptions
import explore.model.enums.WavelengthUnits
import explore.model.itc.ItcTarget
import explore.model.itc.ItcTargetProblem
import explore.model.syntax.all.*
import explore.modes.ConfigSelection
import explore.modes.ItcInstrumentConfig
import explore.modes.ScienceModes
import explore.services.OdbObservationApi
import explore.services.OdbSequenceApi
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.react.primereact.DropdownOptional
import lucuma.react.primereact.SelectItem
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.ObservingMode.*
import lucuma.schemas.odb.input.*
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.effect.*
import lucuma.ui.undo.*
import monocle.Iso
import org.typelevel.log4cats.Logger
import queries.schemas.itc.syntax.*

import scala.collection.immutable.SortedSet

import scalajs.js

final case class ConfigurationTile(
  userId:                   Option[User.Id],
  programId:                Program.Id,
  obsId:                    Observation.Id,
  requirements:             UndoSetter[ScienceRequirements],
  pacAndMode:               UndoSetter[PosAngleConstraintAndObsMode],
  scienceTargetIds:         SortedSet[Target.Id],
  baseCoordinates:          Option[Coordinates],
  obsConf:                  ObsConfiguration,
  selectedConfig:           View[ConfigSelection],
  revertedInstrumentConfig: List[ItcInstrumentConfig], // configuration rows selected if reverted
  modes:                    ScienceModes,
  customSedTimestamps:      List[Timestamp],
  allTargets:               TargetList,
  observingModeGroups:      ObservingModeGroupList,
  sequenceChanged:          Callback,
  readonly:                 Boolean,
  obsIdSetEditInfo:         ObsIdSetEditInfo,          // for determining edit permissions
  units:                    WavelengthUnits,
  isStaffOrAdmin:           Boolean,
  targetView:               View[Option[ItcTarget]],
  hasMaterializedSequence:  Boolean,
  modePending:              Boolean                    // observingMode detail still loading
) extends Tile[ConfigurationTile](
      ObsTabTileIds.ConfigurationId.id,
      "Configuration",
      bodyClass = ExploreStyles.ConfigurationTileBody
    )(ConfigurationTile):
  val observingMode: Option[ObservingMode]                             = pacAndMode.get._2
  val mode: UndoSetter[Option[ObservingMode]]                          =
    pacAndMode.zoom(PosAngleConstraintAndObsMode.observingMode)
  val posAngle: UndoSetter[PosAngleConstraint]                         =
    pacAndMode.zoom(PosAngleConstraintAndObsMode.posAngleConstraint)
  // staff can edit some things for ongoing observations
  val permissions: ConfigEditPermissions                               =
    if readonly || obsIdSetEditInfo.hasCompleted || (obsIdSetEditInfo.hasExecuted && !isStaffOrAdmin)
    then ConfigEditPermissions.Readonly
    else if obsIdSetEditInfo.hasExecuted
    then ConfigEditPermissions.OnlyForOngoing
    else ConfigEditPermissions.FullEdit
  val itcTargets: EitherNec[ItcTargetProblem, NonEmptyList[ItcTarget]] =
    scienceTargetIds.toItcTargets(allTargets)

object ConfigurationTile
    extends TileComponent[ConfigurationTile]({ (props, _) =>
      def isAlienVisitorMode(m: VisitorObservingModeType): Boolean = m match
        case VisitorObservingModeType.VisitorNorth | VisitorObservingModeType.VisitorSouth => true
        case _                                                                             => false

      def pacAndModeAction(
        obsId:  Observation.Id
      )(using
        odbApi: OdbObservationApi[IO]
      ): Action[PosAngleConstraintAndObsMode, PosAngleConstraintAndObsMode] =
        Action[PosAngleConstraintAndObsMode, PosAngleConstraintAndObsMode](
          getter = identity,
          setter = x => _ => x
        )(
          onSet = (_, _) => IO.unit, // Nothing to do, creation is done before this
          onRestore = (_, pm) =>
            val (pac, oMode) = pm

            odbApi.updateObservations(
              List(obsId),
              ObservationPropertiesInput(
                observingMode = oMode.map(_.toInput).orUnassign,
                posAngleConstraint = pac.toInput.assign
              )
            )
        )

      def modeAction(
        obsId:  Observation.Id
      )(using
        odbApi: OdbObservationApi[IO]
      ): Action[Option[ObservingMode], Option[ObservingMode]] =
        Action[Option[ObservingMode], Option[ObservingMode]](
          getter = identity,
          setter = x => _ => x
        )(
          onSet = (_, _) => IO.unit, // Nothing to do, creation is done before this
          onRestore = (_, oMode) =>
            odbApi.updateObservations(
              List(obsId),
              ObservationPropertiesInput(observingMode = oMode.map(_.toInput).orUnassign)
            )
        )

      def checkAndDeleteSequenceIfNeeded(
        obsId:                   Observation.Id,
        hasMaterializedSequence: Boolean,
        action:                  IO[Unit],
        isChanging:              View[IsActive],
        header:                  String
      )(using
        seqApi:                  OdbSequenceApi[IO],
        logger:                  Logger[IO]
      ): IO[Unit] =
        if hasMaterializedSequence
        then
          deleteConfirmation(
            msg =
              "This will delete the customized sequence, which cannot be undone. Do you want to proceed?",
            header = header,
            acceptLabel = "Yes, continue",
            action = seqApi.deleteSequence(obsId) >> action,
            active = isChanging
          ).to[IO]
        else action.switching(isChanging.async, IsActive(_))

      def updateConfiguration(
        obsId:                    Observation.Id,
        hasMaterializedSequence:  Boolean,
        pacAndMode:               UndoSetter[PosAngleConstraintAndObsMode],
        input:                    ObservingModeInput,
        defaultPosAngleConstrait: PosAngleOptions,
        isChanging:               View[IsActive]
      )(using
        obsApi:                   OdbObservationApi[IO],
        seqApi:                   OdbSequenceApi[IO],
        logger:                   Logger[IO]
      ): IO[Unit] =
        val currentPac = pacAndMode.get._1
        val update     = if (defaultPosAngleConstrait != currentPac.toPosAngleOptions)
          val angle  =
            PosAngleConstraint.angle.getOption(currentPac).getOrElse(Angle.Angle0)
          val newPac = defaultPosAngleConstrait.toPosAngle(angle)
          obsApi
            .updateConfiguration(obsId, input.assign, newPac.toInput.assign)
            .flatMap: om =>
              pacAndModeAction(obsId)
                .set(pacAndMode)((newPac, om))
                .toAsync
        else
          obsApi
            .updateConfiguration(obsId, input.assign)
            .flatMap: om =>
              modeAction(obsId)
                .set(pacAndMode.zoom(PosAngleConstraintAndObsMode.observingMode))(om)
                .toAsync
        checkAndDeleteSequenceIfNeeded(obsId,
                                       hasMaterializedSequence,
                                       update,
                                       isChanging,
                                       "Select Configuration"
        )

      def revertConfiguration(
        obsId:                    Observation.Id,
        hasMaterializedSequence:  Boolean,
        mode:                     UndoSetter[Option[ObservingMode]],
        revertedInstrumentConfig: List[ItcInstrumentConfig],
        selectedConfig:           View[ConfigSelection],
        isChanging:               View[IsActive]
      )(using
        odbApi:                   OdbObservationApi[IO],
        seqApi:                   OdbSequenceApi[IO],
        logger:                   Logger[IO]
      ): IO[Unit] =
        val revert = odbApi
          .updateConfiguration(obsId, Input.unassign) >>
          (modeAction(obsId).set(mode)(none) >>
            selectedConfig.set(
              ConfigSelection.fromInstrumentConfigs(revertedInstrumentConfig)
            )).toAsync
        checkAndDeleteSequenceIfNeeded(obsId,
                                       hasMaterializedSequence,
                                       revert,
                                       isChanging,
                                       "Revert Configuration"
        )

      /**
       * Handles the case where `A.Input[B]` not have an assigned value, but it needs to be created
       * for the `mod` function to work on.
       */
      def modInput[A, B](mod: (B => B) => A => A): (B => B) => Input[A] => Input[A] =
        f => inputA => inputA.map(a => mod(f)(a))

      val EmptyGmosNorthLongSlitInput: ObservingModeInput =
        ObservingModeInput.GmosNorthLongSlit(GmosNorthLongSlitInput())
      val EmptyGmosSouthLongSlitInput: ObservingModeInput =
        ObservingModeInput.GmosSouthLongSlit(GmosSouthLongSlitInput())
      val EmptyF2LongSlitInput: ObservingModeInput        =
        ObservingModeInput.Flamingos2LongSlit(Flamingos2LongSlitInput())
      val EmptyIgrins2LongSlitInput: ObservingModeInput   =
        ObservingModeInput.Igrins2LongSlit(Igrins2LongSlitInput())
      val EmptyGnirsSpectroscopyInput: ObservingModeInput =
        ObservingModeInput.GnirsSpectroscopy(GnirsSpectroscopyInput())
      val EmptyGhostIfuInput: ObservingModeInput          =
        ObservingModeInput.GhostIfu(GhostIfuInput())
      val EmptyVisitorInput: ObservingModeInput           =
        ObservingModeInput.Visitor(VisitorInput())
      val EmptyGmosNorthImagingInput: ObservingModeInput  =
        ObservingModeInput.GmosNorthImaging(GmosNorthImagingInput())
      val EmptyGmosSouthImagingInput: ObservingModeInput  =
        ObservingModeInput.GmosSouthImaging(GmosSouthImagingInput())
      val EmptyF2ImagingInput: ObservingModeInput         =
        ObservingModeInput.Flamingos2Imaging(Flamingos2ImagingInput())
      val EmptyGnirsImagingInput: ObservingModeInput      =
        ObservingModeInput.GnirsImaging(GnirsImagingInput())

      for
        ctx        <- useContext(AppContext.ctx)
        isChanging <- useStateView(IsActive(false))
      yield
        import ctx.given

        val revertConfig: IO[Unit] =
          revertConfiguration(
            props.obsId,
            props.hasMaterializedSequence,
            props.mode,
            props.revertedInstrumentConfig,
            props.selectedConfig,
            isChanging
          )

        val title =
          <.div(ExploreStyles.TileTitleConfigSelector)(
            DropdownOptional[ObservingModeSummary](
              value = props.observingMode.map(ObservingModeSummary.fromObservingMode),
              placeholder =
                if props.modePending then "Loading observing mode..."
                else "Choose existing observing mode...",
              disabled = isChanging.get || props.modePending,
              loading = isChanging.get || props.modePending,
              showClear = true,
              onChange = (om: Option[ObservingModeSummary]) =>
                om.fold(revertConfig.runAsync)(m =>
                  updateConfiguration(
                    props.obsId,
                    props.hasMaterializedSequence,
                    props.pacAndMode,
                    m.toInput,
                    m.obsModeType.defaultPosAngleOptions,
                    isChanging
                  ).runAsync
                ),
              panelClass = ExploreStyles.TileTitleConfigSelectorPanel,
              itemTemplate = (om: js.UndefOr[SelectItem[ObservingModeSummary]]) =>
                om.map(si =>
                  <.div(
                    si.value.dropdownEntry
                      .split("\n")
                      .map(<.div(_))
                      .toVdomArray
                  )
                ).getOrElse("None"),
              options = props.observingModeGroups.values.toList.sorted
                .map:
                  _.map: om =>
                    new SelectItem[ObservingModeSummary](value = om, label = om.dropdownEntry)
                .flattenOption
            ).when(props.permissions.isFullEdit)
          )

        val posAngleConstraintAligner: Aligner[PosAngleConstraint, Input[PosAngleConstraintInput]] =
          Aligner(
            props.posAngle,
            UpdateObservationsInput(
              WHERE = props.obsId.toWhereObservation.assign,
              SET = ObservationPropertiesInput()
            ),
            ctx.odbApi.updateObservations(_)
          ).zoom(
            Iso.id,
            UpdateObservationsInput.SET
              .andThen(ObservationPropertiesInput.posAngleConstraint)
              .modify
          )

        val posAngleConstraintView: View[PosAngleConstraint] =
          posAngleConstraintAligner.view(_.toInput.assign)

        def optModeAligner(
          input: ObservingModeInput
        ): Option[Aligner[ObservingMode, Input[ObservingModeInput]]] =
          Aligner(
            props.mode,
            UpdateObservationsInput(
              WHERE = props.obsId.toWhereObservation.assign,
              SET = ObservationPropertiesInput(observingMode = input.assign)
            ),
            (ctx.odbApi.updateObservations(_)).andThen(_.void)
          ).zoom( // Can we avoid the zoom and make an Aligner constructor that takes an input value?
            Iso.id,
            UpdateObservationsInput.SET.andThen(ObservationPropertiesInput.observingMode).modify
          ).toOption

        val optGmosNorthAligner: Option[Aligner[GmosNorthLongSlit, GmosNorthLongSlitInput]] =
          optModeAligner(EmptyGmosNorthLongSlitInput).flatMap:
            _.zoomOpt(
              ObservingMode.gmosNorthLongSlit,
              modInput:
                ObservingModeInput.gmosNorthLongSlit
                  .andThen(ObservingModeInput.GmosNorthLongSlit.value)
                  .modify
            )

        val optGmosSouthAligner: Option[Aligner[GmosSouthLongSlit, GmosSouthLongSlitInput]] =
          optModeAligner(EmptyGmosSouthLongSlitInput).flatMap:
            _.zoomOpt(
              ObservingMode.gmosSouthLongSlit,
              modInput:
                ObservingModeInput.gmosSouthLongSlit
                  .andThen(ObservingModeInput.GmosSouthLongSlit.value)
                  .modify
            )

        val optFlamingos2Aligner: Option[Aligner[Flamingos2LongSlit, Flamingos2LongSlitInput]] =
          optModeAligner(EmptyF2LongSlitInput).flatMap:
            _.zoomOpt(
              ObservingMode.flamingos2LongSlit,
              modInput:
                ObservingModeInput.flamingos2LongSlit
                  .andThen(ObservingModeInput.Flamingos2LongSlit.value)
                  .modify
            )

        val optIgrins2Aligner: Option[Aligner[Igrins2LongSlit, Igrins2LongSlitInput]] =
          optModeAligner(EmptyIgrins2LongSlitInput).flatMap:
            _.zoomOpt(
              ObservingMode.igrins2LongSlit,
              modInput:
                ObservingModeInput.igrins2LongSlit
                  .andThen(ObservingModeInput.Igrins2LongSlit.value)
                  .modify
            )

        val optGnirsImagingAligner: Option[Aligner[GnirsImaging, GnirsImagingInput]] =
          optModeAligner(EmptyGnirsImagingInput).flatMap:
            _.zoomOpt(
              ObservingMode.gnirsImaging,
              modInput:
                ObservingModeInput.gnirsImaging
                  .andThen(ObservingModeInput.GnirsImaging.value)
                  .modify
            )

        val optGnirsSpectroscopyAligner
          : Option[Aligner[GnirsSpectroscopy, GnirsSpectroscopyInput]] =
          optModeAligner(EmptyGnirsSpectroscopyInput).flatMap:
            _.zoomOpt(
              ObservingMode.gnirsSpectroscopy,
              modInput:
                ObservingModeInput.gnirsSpectroscopy
                  .andThen(ObservingModeInput.GnirsSpectroscopy.value)
                  .modify
            )

        val optGhostIfuAligner: Option[Aligner[GhostIfu, GhostIfuInput]] =
          optModeAligner(EmptyGhostIfuInput).flatMap:
            _.zoomOpt(
              ObservingMode.ghostIfu,
              modInput:
                ObservingModeInput.ghostIfu
                  .andThen(ObservingModeInput.GhostIfu.value)
                  .modify
            )

        val optVisitorAligner: Option[Aligner[ObservingMode.Visitor, VisitorInput]] =
          optModeAligner(EmptyVisitorInput).flatMap:
            _.zoomOpt(
              ObservingMode.visitor,
              modInput:
                ObservingModeInput.visitor
                  .andThen(ObservingModeInput.Visitor.value)
                  .modify
            )

        val optGmosNorthImagingAligner: Option[Aligner[GmosNorthImaging, GmosNorthImagingInput]] =
          optModeAligner(EmptyGmosNorthImagingInput).flatMap:
            _.zoomOpt(
              ObservingMode.gmosNorthImaging,
              modInput:
                ObservingModeInput.gmosNorthImaging
                  .andThen(ObservingModeInput.GmosNorthImaging.value)
                  .modify
            )

        val optGmosSouthImagingAligner: Option[Aligner[GmosSouthImaging, GmosSouthImagingInput]] =
          optModeAligner(EmptyGmosSouthImagingInput).flatMap:
            _.zoomOpt(
              ObservingMode.gmosSouthImaging,
              modInput:
                ObservingModeInput.gmosSouthImaging
                  .andThen(ObservingModeInput.GmosSouthImaging.value)
                  .modify
            )

        val optFlamingos2ImagingAligner
          : Option[Aligner[Flamingos2Imaging, Flamingos2ImagingInput]] =
          optModeAligner(EmptyF2ImagingInput).flatMap:
            _.zoomOpt(
              ObservingMode.flamingos2Imaging,
              modInput:
                ObservingModeInput.flamingos2Imaging
                  .andThen(ObservingModeInput.Flamingos2Imaging.value)
                  .modify
            )

        val requirementsViewSet: ScienceRequirementsUndoView =
          ScienceRequirementsUndoView(props.obsId, props.requirements)

        val requirementsView: View[ScienceRequirements] =
          requirementsViewSet(
            Iso.id.asLens,
            UpdateScienceRequirements.scienceRequirements
          )

        val reqsExposureTimeMode: Option[ExposureTimeMode] =
          props.requirements.get.exposureTimeMode

        val body =
          React.Fragment(
            <.div(ExploreStyles.ConfigurationGrid)(
              props.obsConf.agsState
                .map(agsState =>
                  PAConfigurationPanel(
                    props.programId,
                    props.obsId,
                    posAngleConstraintView,
                    props.obsConf.selectedPA,
                    props.obsConf.averagePA,
                    agsState,
                    props.permissions
                  )
                ),
              if (props.mode.get.isEmpty)
                props.obsConf.constraints
                  .map(constraints =>
                    BasicConfigurationPanel(
                      props.userId,
                      props.obsId,
                      requirementsView,
                      props.selectedConfig,
                      constraints,
                      props.itcTargets,
                      props.baseCoordinates,
                      props.obsConf.calibrationRole,
                      (input, posAngleOptions) =>
                        updateConfiguration(
                          props.obsId,
                          props.hasMaterializedSequence,
                          props.pacAndMode,
                          input,
                          posAngleOptions,
                          isChanging
                        ),
                      props.modes,
                      props.customSedTimestamps,
                      !props.permissions.isFullEdit,
                      props.units,
                      props.targetView
                    )
                  )
              else
                React.Fragment(
                  // Gmos North Long Slit
                  optGmosNorthAligner.map: northAligner =>
                    GmosLongslitConfigPanel
                      .GmosNorthLongSlit(
                        props.programId,
                        props.obsId,
                        props.obsConf.calibrationRole,
                        northAligner,
                        revertConfig,
                        props.modes.spectroscopy,
                        props.sequenceChanged,
                        props.permissions,
                        props.units
                      ),
                  // Gmos South Long Slit
                  optGmosSouthAligner.map: southAligner =>
                    GmosLongslitConfigPanel
                      .GmosSouthLongSlit(
                        props.programId,
                        props.obsId,
                        props.obsConf.calibrationRole,
                        southAligner,
                        revertConfig,
                        props.modes.spectroscopy,
                        props.sequenceChanged,
                        props.permissions,
                        props.units
                      ),
                  // Gmos North Imaging
                  optGmosNorthImagingAligner.map: aligner =>
                    GmosImagingConfigPanel.GmosNorthImaging(
                      props.programId,
                      props.obsId,
                      props.obsConf.calibrationRole,
                      aligner,
                      reqsExposureTimeMode,
                      revertConfig,
                      props.sequenceChanged,
                      !props.permissions.isFullEdit,
                      props.units
                    ),
                  // Gmos South Imaging
                  optGmosSouthImagingAligner.map: aligner =>
                    GmosImagingConfigPanel.GmosSouthImaging(
                      props.programId,
                      props.obsId,
                      props.obsConf.calibrationRole,
                      aligner,
                      reqsExposureTimeMode,
                      revertConfig,
                      props.sequenceChanged,
                      !props.permissions.isFullEdit,
                      props.units
                    ),
                  // Flamingos2 Long Slit
                  optFlamingos2Aligner.map: f2Aligner =>
                    Flamingos2LongslitConfigPanel(
                      props.programId,
                      props.obsId,
                      props.obsConf.calibrationRole,
                      f2Aligner,
                      revertConfig,
                      props.modes.spectroscopy,
                      props.sequenceChanged,
                      props.permissions,
                      props.units,
                      props.isStaffOrAdmin
                    ),
                  // Flamingos2 Imaging
                  optFlamingos2ImagingAligner.map: f2ImgAligner =>
                    Flamingos2ImagingConfigPanel(
                      props.programId,
                      props.obsId,
                      props.obsConf.calibrationRole,
                      f2ImgAligner,
                      reqsExposureTimeMode,
                      revertConfig,
                      props.sequenceChanged,
                      !props.permissions.isFullEdit,
                      props.units,
                      props.isStaffOrAdmin
                    ),
                  // IGRINS2 Long Slit
                  optIgrins2Aligner.map: ig2Aligner =>
                    Igrins2LongslitConfigPanel(
                      props.programId,
                      props.obsId,
                      props.obsConf.calibrationRole,
                      ig2Aligner,
                      revertConfig,
                      props.modes.spectroscopy,
                      props.sequenceChanged,
                      props.permissions,
                      props.units
                    ),
                  // GNIRS Imaging
                  optGnirsImagingAligner.map: gnirsImgAligner =>
                    GnirsImagingConfigPanel(
                      props.programId,
                      props.obsId,
                      props.obsConf.calibrationRole,
                      gnirsImgAligner,
                      reqsExposureTimeMode,
                      revertConfig,
                      props.sequenceChanged,
                      !props.permissions.isFullEdit,
                      props.units,
                      props.isStaffOrAdmin
                    ),
                  // GNIRS Long Slit
                  optGnirsSpectroscopyAligner.map: gnirsAligner =>
                    GnirsSpectroscopyPanel(
                      props.programId,
                      props.obsId,
                      props.obsConf.calibrationRole,
                      gnirsAligner,
                      revertConfig,
                      props.modes.spectroscopy,
                      props.sequenceChanged,
                      props.permissions,
                      props.isStaffOrAdmin,
                      props.units
                    ),
                  // Ghost IFU
                  optGhostIfuAligner.map: ghostAligner =>
                    GhostIfuConfigPanel(
                      props.programId,
                      props.obsId,
                      props.obsConf.calibrationRole,
                      ghostAligner,
                      revertConfig,
                      props.sequenceChanged,
                      props.permissions,
                      props.units
                    ),
                  // Resident Visitor (Alopeke / Zorro / maroon-x).
                  optVisitorAligner
                    .filterNot(a => isAlienVisitorMode(a.get.mode))
                    .map: visitorAligner =>
                      ResidentVisitorConfigPanel(
                        props.programId,
                        props.obsId,
                        visitorAligner,
                        requirementsView,
                        revertConfig,
                        props.permissions,
                        props.units
                      ),
                  // Alien visitors (VisitorNorth / VisitorSouth).
                  optVisitorAligner
                    .filter(a => isAlienVisitorMode(a.get.mode))
                    .map: visitorAligner =>
                      AlienVisitorConfigPanel(
                        props.programId,
                        props.obsId,
                        visitorAligner,
                        revertConfig,
                        props.permissions,
                        props.units
                      )
                )
            )
          )

        TileContents(title, body)
    })
