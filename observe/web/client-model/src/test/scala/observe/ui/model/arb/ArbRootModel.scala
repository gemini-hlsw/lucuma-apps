// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model.arb

import cats.Order.given
import crystal.Pot
import crystal.arb.given
import eu.timepit.refined.scalacheck.string.given
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.math.arb.ArbRefined.given
import lucuma.core.model.GuideConfig
import lucuma.core.model.Observation
import lucuma.core.model.arb.ArbGuideConfig.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.core.util.arb.ArbNewType.given
import lucuma.react.table.ColumnFilters
import lucuma.react.table.ColumnId
import lucuma.ui.sequence.SelectedRowId
import lucuma.ui.sequence.arb.ArbSelectedRowId.given
import lucuma.ui.sso.UserVault
import lucuma.ui.sso.arb.ArbUserVault.given
import observe.common.FixedLengthBuffer
import observe.common.arb.ArbFixedLengthBuffer.given
import observe.model.CurrentConditions
import observe.model.ExecutionState
import observe.model.LogMessage
import observe.model.Observer
import observe.model.Operator
import observe.model.StepProgress
import observe.model.arb.ArbExecutionState.given
import observe.model.arb.ArbLogMessage.given
import observe.model.arb.ArbObsRecordedIds.given
import observe.model.arb.ArbStepProgress.given
import observe.model.arb.ObserveModelArbitraries.given
import observe.model.odb.ObsRecordedIds
import observe.ui.model.IsAudioActivated
import observe.ui.model.LoadedObservations
import observe.ui.model.UserPreferences
import lucuma.ui.enums.Theme
import observe.model.enums.ObserveLogLevel
import observe.ui.model.ObsSummary
import observe.ui.model.ObservationRequests
import observe.ui.model.RootModelData
import observe.ui.model.arb.ArbLoadedObservation.given
import observe.ui.model.arb.ArbObsSummary.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import ArbObservationRequests.given

trait ArbRootModel:
  // Make sure a known Observation.Id is generated somewhere.
  given Arbitrary[Observation.Id] = Arbitrary:
    Gen.oneOf(Gen.const(StandardObsId), arbGid[Observation.Id].arbitrary)

  given Arbitrary[ColumnFilters] = Arbitrary:
    arbitrary[Map[String, String]].map(m => ColumnFilters(m.map((k, v) => ColumnId(k) -> v)))

  given Arbitrary[UserPreferences] = Arbitrary:
    for
      audio <- arbitrary[IsAudioActivated]
      theme <- Gen.oneOf(Theme.values*)
      level <- Gen.oneOf(ObserveLogLevel.values*)
      utc   <- arbitrary[Boolean]
      gf    <- arbitrary[String]
      cf    <- arbitrary[ColumnFilters]
    yield UserPreferences(audio, theme, level, utc, gf, cf)

  given Cogen[UserPreferences] =
    Cogen[(IsAudioActivated, Int, Int, Boolean, String, Map[String, String])].contramap: p =>
      (p.isAudioActivated, p.theme.ordinal, p.logLevel.ordinal, p.logTimeIsUTC,
       p.obsListGlobalFilter, p.obsListColumnFilters.value.map((k, v) => k.value -> v.toString))

  given Arbitrary[RootModelData] = Arbitrary:
    for
      uv    <- arbitrary[Pot[Option[UserVault]]]
      ros   <- arbitrary[Pot[List[ObsSummary]]]
      los   <- arbitrary[LoadedObservations]
      es    <- arbitrary[Map[Observation.Id, ExecutionState]]
      ri    <- arbitrary[ObsRecordedIds]
      sp    <- arbitrary[Map[Observation.Id, StepProgress]]
      usr   <- arbitrary[Map[Observation.Id, SelectedRowId]]
      or    <- arbitrary[Map[Observation.Id, ObservationRequests]]
      cs    <- arbitrary[CurrentConditions]
      gc    <- arbitrary[GuideConfig]
      obs   <- arbitrary[Option[Observer]]
      op    <- arbitrary[Option[Operator]]
      usm   <- arbitrary[Option[NonEmptyString]]
      log   <- arbitrary[FixedLengthBuffer[LogMessage]]
      up    <- arbitrary[UserPreferences]
    yield RootModelData(
      uv,
      ros,
      los,
      es,
      ri,
      sp,
      usr,
      or,
      cs,
      gc,
      obs,
      op,
      usm,
      log,
      up
    )

  given Cogen[RootModelData] = Cogen[
    (
      Pot[Option[UserVault]],
      Pot[List[ObsSummary]],
      LoadedObservations,
      Map[Observation.Id, ExecutionState],
      ObsRecordedIds,
      Map[Observation.Id, StepProgress],
      Map[Observation.Id, SelectedRowId],
      Map[Observation.Id, ObservationRequests],
      CurrentConditions,
      GuideConfig,
      Option[Observer],
      Option[Operator],
      Option[NonEmptyString],
      FixedLengthBuffer[LogMessage],
      UserPreferences
    )
  ].contramap: x =>
    (x.userVault,
     x.obsList,
     x.loadedObservations,
     x.executionState,
     x.recordedIds,
     x.obsProgress,
     x.userSelectedRow,
     x.obsRequests,
     x.conditions,
     x.guideConfig,
     x.observer,
     x.operator,
     x.userSelectionMessage,
     x.globalLog,
     x.userPreferences
    )

object ArbRootModel extends ArbRootModel
