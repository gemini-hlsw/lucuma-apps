// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.effect.Async
import cats.effect.Resource
import cats.implicits.*
import clue.StreamingClient
import clue.data.Input
import clue.data.syntax.*
import clue.syntax.*
import crystal.Pot
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.Observation
import explore.model.SchedulingConstraints
import explore.utils.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Coordinates
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Group
import lucuma.core.model.ObservationReference
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.enums.BlindOffsetType
import lucuma.schemas.odb.input.*
import queries.common.ObsQueriesGQL.*
import queries.common.ProgramSummaryQueriesGQL.AllProgramObservations
import queries.common.ProgramSummaryQueriesGQL.AllProgramObservationsObservingMode
import queries.common.TargetQueriesGQL.SetGuideTargetName

import java.time.Instant
import scala.concurrent.duration.*

trait OdbObservationApiImpl[F[_]: Async](using StreamingClient[F, ObservationDB])
    extends OdbObservationApi[F]:
  self: OdbApiHelper[F] =>

  def updateObservations(input: UpdateObservationsInput): F[Unit] =
    UpdateObservationMutation[F]
      .execute(input)
      .processErrors
      .void

  def updateObservations(
    obsIds: List[Observation.Id],
    input:  ObservationPropertiesInput
  ): F[Unit] =
    updateObservations:
      UpdateObservationsInput(
        WHERE = obsIds.toWhereObservation.assign,
        SET = input
      )

  def updateObservationConstraintSet(
    obsIds:      List[Observation.Id],
    constraints: ConstraintSet
  ): F[Unit] = {
    val createER: ElevationRangeInput = constraints.elevationRange match
      case ElevationRange.ByAirMass(min, max)   =>
        ElevationRangeInput(airMass =
          // These are actually safe, because min and max in the model are refined [1.0 - 3.0]
          AirMassRangeInput(
            min = PosBigDecimal.unsafeFrom(min.toBigDecimal).assign,
            max = PosBigDecimal.unsafeFrom(max.toBigDecimal).assign
          ).assign
        )
      case ElevationRange.ByHourAngle(min, max) =>
        ElevationRangeInput(hourAngle =
          HourAngleRangeInput(
            minHours = min.toBigDecimal.assign,
            maxHours = max.toBigDecimal.assign
          ).assign
        )

    val editInput = ObservationPropertiesInput(
      constraintSet = ConstraintSetInput(
        imageQuality = constraints.imageQuality.assign,
        cloudExtinction = constraints.cloudExtinction.assign,
        skyBackground = constraints.skyBackground.assign,
        waterVapor = constraints.waterVapor.assign,
        elevationRange = createER.assign
      ).assign
    )

    updateObservations(obsIds, editInput)
  }

  def updateVisualizationTime(
    obsIds:          List[Observation.Id],
    observationTime: Option[Instant]
  ): F[Unit] = {
    val editInput = ObservationTimesInput(observationTime =
      observationTime.flatMap(Timestamp.fromInstantTruncated).orUnassign
    )

    UpdateObservationTimesMutation[F]
      .execute:
        UpdateObservationsTimesInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .processErrors
      .void
  }

  def updateVisualizationDuration(
    obsIds:              List[Observation.Id],
    observationDuration: Option[TimeSpan]
  ): F[Unit] = {
    val editInput = ObservationTimesInput(
      observationDuration = observationDuration.map(_.toInput).orUnassign
    )

    UpdateObservationTimesMutation[F]
      .execute:
        UpdateObservationsTimesInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .processErrors
      .void
  }

  def updateVisualizationTimeAndDuration(
    obsIds:              List[Observation.Id],
    observationTime:     Option[Instant],
    observationDuration: Option[TimeSpan]
  ): F[Unit] = {
    val editInput =
      ObservationTimesInput(
        observationTime = observationTime.flatMap(Timestamp.fromInstantTruncated).orUnassign,
        observationDuration = observationDuration.map(_.toInput).orUnassign
      )

    UpdateObservationTimesMutation[F]
      .execute:
        UpdateObservationsTimesInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .processErrors
      .void
  }

  def updatePosAngle(
    obsIds:             List[Observation.Id],
    posAngleConstraint: PosAngleConstraint
  ): F[Unit] = {
    val editInput =
      ObservationPropertiesInput(posAngleConstraint = posAngleConstraint.toInput.assign)

    UpdateObservationMutation[F]
      .execute:
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .processErrors
      .void
  }

  def updateGhostIfu2SkyPosition(
    obsIds:      List[Observation.Id],
    skyPosition: Option[Coordinates]
  ): F[Unit] = {
    val editInput = ObservationPropertiesInput(observingMode =
      ObservingModeInput
        .GhostIfu(
          GhostIfuInput(skyPosition =
            skyPosition
              .map(c => CoordinatesInput(ra = c.ra.toInput.assign, dec = c.dec.toInput.assign))
              .orUnassign
          )
        )
        .assign
    )

    updateObservations(obsIds, editInput)
  }

  def updateNotes(
    obsIds: List[Observation.Id],
    notes:  Option[NonEmptyString]
  ): F[Unit] = {
    val editInput = ObservationPropertiesInput(observerNotes = notes.orUnassign)

    UpdateObservationMutation[F]
      .execute:
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .processErrors
      .void
  }

  def createObservation(
    programId: Program.Id,
    parentId:  Option[Group.Id]
  ): F[Observation] =
    ProgramCreateObservation[F]
      .execute:
        CreateObservationInput(
          programId = programId.assign,
          SET = parentId
            .map(gId => ObservationPropertiesInput(groupId = gId.assign))
            .orIgnore
        )
      .processNoDataErrors
      .map: result =>
        result.createObservation.observation

  def createObservationWithTargets(
    programId: Program.Id,
    targetIds: Set[Target.Id]
  ): F[Observation] =
    ProgramCreateObservation[F]
      .execute:
        CreateObservationInput(
          programId = programId.assign,
          SET = ObservationPropertiesInput(
            targetEnvironment = TargetEnvironmentInput(asterism = targetIds.toList.assign).assign
          ).assign
        )
      .processErrors
      .map(_.createObservation.observation)

  // Re-hydrate an observation with a one query call to fill the blanks of teh subquery.
  private def hydrateObservingMode(obs: Observation): F[Observation] =
    obs.basicConfiguration.fold(obs.pure[F]): bc =>
      observationObservingMode(obs.id, bc.obsModeType).map: mode =>
        Observation.observingMode.replace(Pot.Ready(mode))(obs)

  def cloneObservation(
    obsId:      Observation.Id,
    newGroupId: Option[Group.Id]
  ): F[Observation] =
    CloneObservationMutation[F]
      .execute:
        CloneObservationInput(
          observationId = obsId.assign,
          SET = ObservationPropertiesInput(groupId = newGroupId.orUnassign).assign
        )
      .processNoDataErrors
      .map(_.cloneObservation.newObservation)
      .flatMap(hydrateObservingMode)

  def applyObservation(
    obsId:                   Observation.Id,
    onTargets:               Option[List[Target.Id]] = none,
    onConstraintSet:         Option[ConstraintSet] = none,
    onSchedulingConstraints: Option[SchedulingConstraints] = none
  ): F[Observation] =
    CloneObservationMutation[F]
      .execute:
        CloneObservationInput(
          observationId = obsId.assign,
          SET = ObservationPropertiesInput(
            targetEnvironment =
              onTargets.map(tids => TargetEnvironmentInput(asterism = tids.assign)).orIgnore,
            constraintSet = onConstraintSet.map(_.toInput).orIgnore,
            schedulingConstraints = onSchedulingConstraints.map(_.toInput).orIgnore,
            attachments = List.empty.assign // Always clean observation attachments
          ).assign
        )
      .processErrorsIgnoring(ignorePendingObsCalc)
      .map(_.cloneObservation.newObservation)
      .flatMap(hydrateObservingMode)

  def deleteObservation(obsId: Observation.Id): F[Unit] =
    deleteObservations(List(obsId))

  def undeleteObservation(obsId: Observation.Id): F[Unit] =
    undeleteObservations(List(obsId))

  def deleteObservations(obsIds: List[Observation.Id]): F[Unit] =
    UpdateObservationMutation[F]
      .execute:
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = ObservationPropertiesInput(existence = Existence.Deleted.assign)
        )
      .processErrors
      .void

  def undeleteObservations(obsIds: List[Observation.Id]): F[Unit] =
    UpdateObservationMutation[F]
      .execute:
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = ObservationPropertiesInput(existence = Existence.Present.assign),
          includeDeleted = true.assign
        )
      .processErrors
      .void

  /**
   * @param programId
   * @param obsId
   * @param groupId
   *   Group to move to. `None` to move to top level
   * @param groupIndex
   *   New index in group. `None` to leave position unchanged
   */
  def moveObservation(
    obsId:      Observation.Id,
    groupId:    Option[Group.Id],
    groupIndex: NonNegShort
  ): F[Unit] =
    val input = UpdateObservationsInput(
      WHERE = obsId.toWhereObservation.assign,
      SET = ObservationPropertiesInput(
        groupId = groupId.orUnassign,
        groupIndex = groupIndex.assign
      )
    )
    UpdateObservationMutation[F].execute(input).processErrors.void

  def setGuideTargetName(
    obsId:      Observation.Id,
    targetName: Option[NonEmptyString]
  ): F[Unit] =
    val input = SetGuideTargetNameInput(
      observationId = obsId.assign,
      targetName = targetName.orUnassign
    )
    SetGuideTargetName[F].execute(input).processErrors.void

  def guideTargetName(obsId: Observation.Id): F[Option[NonEmptyString]] =
    ObservationLoadedElements[F]
      .query(obsId)
      .processErrors
      .map(_.observation.flatMap(_.targetEnvironment.guideTargetName))

  def createConfigurationRequest(
    obsId:         Observation.Id,
    justification: Option[NonEmptyString]
  ): F[ConfigurationRequest] =
    val input = CreateConfigurationRequestInput(
      observationId = obsId.assign,
      SET = ConfigurationRequestProperties(justification = justification.orIgnore).assign
    )
    CreateConfigurationRequestMutation[F].execute(input).processErrors.map(_._1)

  def updateConfiguration(
    obsId:              Observation.Id,
    observingMode:      Input[ObservingModeInput],
    posAngleConstraint: Input[PosAngleConstraintInput] = Input.ignore
  ): F[Option[ObservingMode]] =
    val input = UpdateObservationsInput(
      WHERE = obsId.toWhereObservation.assign,
      SET = ObservationPropertiesInput(
        observingMode = observingMode,
        posAngleConstraint = posAngleConstraint
      )
    )

    UpdateConfigurationMutation[F]
      .execute(input)
      .processErrors
      .map(_.updateObservations.observations.headOption.flatMap(_.observingMode))

  def setObservationWorkflowState(obsId: Observation.Id, st: ObservationWorkflowState): F[Unit] =
    SetObservationWorkflowStateMutation[F]
      .execute:
        SetObservationWorkflowStateInput(obsId, st)
      .processErrors
      .void

  def resolveObservationReference(
    obsRef: ObservationReference
  ): F[Option[(Program.Id, Observation.Id)]] =
    ResolveObsReference[F]
      .query(obsRef.assign)
      .processErrors
      .map(_.observation.map(r => (r.program.id, r.id)))

  def observationEditSubscription(
    obsId: Observation.Id
  ): Resource[F, fs2.Stream[F, Unit]] =
    ObservationEditSubscription
      .subscribe[F](obsId.toObservationEditInput)
      .raiseFirstNoDataError
      .ignoreGraphQLErrors
      .map: // TODO Do we want throttle in all subscriptions? Parametrize throttle timeout?
        _.void.throttle(5.seconds)

  def programObservationsDeltaSubscription(
    programId: Program.Id
  ): Resource[F, fs2.Stream[F, ProgramObservationsDelta.Data.ObservationEdit]] =
    ProgramObservationsDelta
      .subscribe[F](programId.toObservationEditInput)
      .processErrors("ProgramObservationsDelta", ignorePendingObsCalc)
      .map(_.map(_.observationEdit))

  def allProgramObservations(programId: Program.Id): F[List[Observation]] =
    drain[Observation, Observation.Id, AllProgramObservations.Data.Observations](
      offset =>
        AllProgramObservations[F]
          .query(programId.toWhereObservation, offset.orUnassign)
          // We need this because we currently get errors for things like having no targets
          .processNoDataErrors
          .map(_.observations),
      _.matches,
      _.hasMore,
      _.id
    )

  // One flag per `ObservingMode`, for the `@include` directives in `ObservingModeByTypeSubquery`
  private case class ModeViewFlags(
    gmosNorthLongSlit:  Boolean = false,
    gmosSouthLongSlit:  Boolean = false,
    gmosNorthImaging:   Boolean = false,
    gmosSouthImaging:   Boolean = false,
    flamingos2Imaging:  Boolean = false,
    flamingos2LongSlit: Boolean = false,
    igrins2LongSlit:    Boolean = false,
    gnirsImaging:       Boolean = false,
    gnirsSpectroscopy:  Boolean = false,
    ghostIfu:           Boolean = false,
    visitor:            Boolean = false,
    exchange:           Boolean = false
  )

  // Every ObservingModeType maps to exactly one `ObservingMode` union view.
  // We turn on only that view's `@include` flag so the server resolves a single mode-view.
  private def modeViewFlagsFor(modeType: ObservingModeType): ModeViewFlags =
    modeType match
      case ObservingModeType.GmosNorthLongSlit                               =>
        ModeViewFlags(gmosNorthLongSlit = true)
      case ObservingModeType.GmosSouthLongSlit                               =>
        ModeViewFlags(gmosSouthLongSlit = true)
      case ObservingModeType.GmosNorthImaging                                =>
        ModeViewFlags(gmosNorthImaging = true)
      case ObservingModeType.GmosSouthImaging                                =>
        ModeViewFlags(gmosSouthImaging = true)
      case ObservingModeType.Flamingos2Imaging                               =>
        ModeViewFlags(flamingos2Imaging = true)
      case ObservingModeType.Flamingos2LongSlit                              =>
        ModeViewFlags(flamingos2LongSlit = true)
      case ObservingModeType.Igrins2LongSlit                                 =>
        ModeViewFlags(igrins2LongSlit = true)
      case ObservingModeType.GnirsImaging                                    =>
        ModeViewFlags(gnirsImaging = true)
      case ObservingModeType.GnirsLongSlit | ObservingModeType.GnirsIfu      =>
        ModeViewFlags(gnirsSpectroscopy = true)
      case ObservingModeType.GhostIfu                                        =>
        ModeViewFlags(ghostIfu = true)
      case ObservingModeType.ExchangeKeck | ObservingModeType.ExchangeSubaru =>
        ModeViewFlags(exchange = true)
      case ObservingModeType.AlopekeSpeckle | ObservingModeType.AlopekeWideField |
          ObservingModeType.MaroonX | ObservingModeType.VisitorNorth |
          ObservingModeType.VisitorSouth | ObservingModeType.ZorroSpeckle |
          ObservingModeType.ZorroWideField =>
        ModeViewFlags(visitor = true)

  private def fetchObservingModes(
    where:    WhereObservation,
    modeType: ObservingModeType
  ): F[List[(Observation.Id, Option[ObservingMode])]] =
    val flags: ModeViewFlags = modeViewFlagsFor(modeType)
    drain[
      AllProgramObservationsObservingMode.Data.Observations.Matches,
      Observation.Id,
      AllProgramObservationsObservingMode.Data.Observations
    ](
      offset =>
        AllProgramObservationsObservingMode[F]
          .query(
            where,
            offset.orUnassign,
            includeGmosNorthLongSlit = flags.gmosNorthLongSlit,
            includeGmosSouthLongSlit = flags.gmosSouthLongSlit,
            includeGmosNorthImaging = flags.gmosNorthImaging,
            includeGmosSouthImaging = flags.gmosSouthImaging,
            includeFlamingos2Imaging = flags.flamingos2Imaging,
            includeFlamingos2LongSlit = flags.flamingos2LongSlit,
            includeIgrins2LongSlit = flags.igrins2LongSlit,
            includeGnirsImaging = flags.gnirsImaging,
            includeGnirsSpectroscopy = flags.gnirsSpectroscopy,
            includeGhostIfu = flags.ghostIfu,
            includeVisitor = flags.visitor,
            includeExchange = flags.exchange
          )
          .processNoDataErrors
          .map(_.observations),
      _.matches,
      _.hasMore,
      _.id
    ).map(_.map(m => (m.id, m.observingMode)))

  def programObservationsObservingModes(
    programId: Program.Id,
    modeType:  ObservingModeType
  ): F[List[(Observation.Id, Option[ObservingMode])]] =
    fetchObservingModes(
      WhereObservation(
        program = programId.toWhereProgram.assign,
        observingModeType = WhereOptionEqObservingModeType(EQ = modeType.assign).assign
      ),
      modeType
    )

  def observationObservingMode(
    obsId:    Observation.Id,
    modeType: ObservingModeType
  ): F[Option[ObservingMode]] =
    fetchObservingModes(obsId.toWhereObservation, modeType).map(_.headOption.flatMap(_._2))

  def obsCalcSubscription(
    programId: Program.Id
  ): Resource[F, fs2.Stream[F, ObsCalcSubscription.Data.ObscalcUpdate]] =
    ObsCalcSubscription
      .subscribe[F](ObscalcUpdateInput(programId.assign))
      .processErrors("ObsCalcSubscription", ignorePendingObsCalc)
      .map(_.map(_.obscalcUpdate))

  def setBlindOffsetTarget(
    obsId:           Observation.Id,
    target:          Target,
    blindOffsetType: BlindOffsetType
  ): F[Target.Id] =
    val optId: F[Option[Target.Id]] = SetBlindOffsetMutation[F]
      .execute(
        obsId.toWhereObservation,
        true,
        target.toTargetPropertiesInput.assign,
        blindOffsetType
      )
      .processErrors
      .map(
        _.updateObservations.observations.headOption
          .flatMap(_.targetEnvironment.blindOffsetTarget)
          .map(_.id)
      )
    // Since we just set the blind offset and didn't get an error, we should have a Target.Id
    optId.flatMap:
      case None        =>
        Async[F].raiseError(
          new Exception("Unexpected error: Target Id not return for blind offset creation.")
        )
      case Some(value) => value.pure

  def deleteBlindOffsetTarget(obsId: Observation.Id): F[Unit] =
    // The blind offset type doesn't really matter here.
    // Also the API deletes the target if we set useBlindOffset to false, but it doesn't
    // hurt to do it explicitly.
    SetBlindOffsetMutation[F]
      .execute(obsId.toWhereObservation, false, Input.unassign, BlindOffsetType.Automatic)
      .processErrors
      .void

  def initializeAutomaticBlindOffset(obsId: Observation.Id): F[Unit] =
    // By setting useBlindOffset to true and blindOffsetType to automatic, the target
    // we be set by whomever is doing the automatic target assignments. At the time of this
    // writing, that is explore.
    SetBlindOffsetMutation[F]
      .execute(obsId.toWhereObservation, true, Input.unassign, BlindOffsetType.Automatic)
      .processErrors
      .void
