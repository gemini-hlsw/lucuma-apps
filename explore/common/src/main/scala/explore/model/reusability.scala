// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyChain
import cats.syntax.all.*
import clue.PersistentClientStatus
import explore.data.KeyedIndexedList
import explore.model.ScienceRequirements.*
import explore.model.enums.AgsState
import explore.model.enums.GroupWarning
import explore.model.enums.SelectedPanel
import explore.model.itc.ItcExposureTime
import explore.model.itc.ItcQueryProblem
import explore.model.itc.ItcTarget
import explore.model.itc.ItcTargetProblem
import explore.modes.ConfigSelection
import explore.modes.InstrumentOverrides
import explore.modes.ItcInstrumentConfig
import explore.undo.UndoStacks
import explore.utils.OdbRestClient
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.ags.AgsParams
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.catalog.AngularSize
import lucuma.catalog.CatalogTargetResult
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.math.Arc
import lucuma.core.math.Offset
import lucuma.core.math.SignalToNoise
import lucuma.core.math.WavelengthDither
import lucuma.core.model.Configuration
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.TimingWindow
import lucuma.core.model.Tracking
import lucuma.core.model.sequence.Atom
import lucuma.itc.ItcCcd
import lucuma.itc.client.GraphResult
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.model.*
import lucuma.ui.reusability.given
import lucuma.ui.sequence.SequenceRow

/**
 * Reusability instances for model classes
 */
object reusability:
  // Model
  given Reusability[ProgramSummaries]                                         = Reusability.byEq
  given Reusability[ItcTarget]                                                = Reusability.byEq
  given Reusability[ItcTargetProblem]                                         = Reusability.byEq
  given Reusability[ItcQueryProblem]                                          = Reusability.byEq
  given Reusability[PersistentClientStatus]                                   = Reusability.byEq
  given Reusability[AsterismVisualOptions]                                    = Reusability.byEq
  given Reusability[ExpandedIds]                                              = Reusability.byEq
  given Reusability[RootModel]                                                = Reusability.byEq
  given idListReuse[Id, A: Reusability]: Reusability[KeyedIndexedList[Id, A]] =
    Reusability.by(_.toList)

  given Reusability[ObsIdSet]               = Reusability.byEq
  given Reusability[Proposal]               = Reusability.byEq
  given Reusability[ProposalType]           = Reusability.byEq
  given Reusability[TargetEditObsInfo]      = Reusability.byEq
  given Reusability[TargetIdSet]            = Reusability.byEq
  given Reusability[TargetWithId]           = Reusability.byEq
  given Reusability[TargetWithObs]          = Reusability.byEq
  given Reusability[ConstraintGroup]        = Reusability.byEq
  given Reusability[Observation]            = Reusability.byEq
  given Reusability[Attachment]             = Reusability.byEq
  given Reusability[ProgramInfo]            = Reusability.byEq
  given Reusability[ProgramDetails]         = Reusability.byEq
  given Reusability[Execution]              = Reusability.byEq
  given Reusability[BandedProgramTime]      = Reusability.byEq
  given Reusability[Group]                  = Reusability.byEq
  given Reusability[GroupWarning]           = Reusability.byEq
  given [V: Eq]: Reusability[Perishable[V]] = Reusability.byEq

  /**
   */
  given Reusability[PosAngleConstraint] = Reusability.byEq

  // Undo
  given undoStacksReuse[F[_], M]: Reusability[UndoStacks[F, M]] =
    Reusability.by(s => (s.undo.length, s.redo.length, s.working))

  given undoStacksMapReuse[F[_], K, M]: Reusability[Map[K, UndoStacks[F, M]]] =
    Reusability.by[Map[K, UndoStacks[F, M]], Int](_.size) && Reusability[Map[K, UndoStacks[F, M]]](
      (a, b) =>
        a.forall { case (k, stacksA) =>
          b.get(k).exists(stacksB => undoStacksReuse.test(stacksA, stacksB))
        }
    )

  given modelUndoStacksReuse[F[_]]: Reusability[ModelUndoStacks[F]] = Reusability.byEq

  given [A: Eq]: Reusability[Arc[A]]                    = Reusability.byEq
  given Reusability[Progress]                           = Reusability.byEq
  given Reusability[AngularSize]                        = Reusability.byEq
  given Reusability[CatalogTargetResult]                = Reusability.byEq
  given Reusability[BasicConfiguration]                 = Reusability.byEq
  given Reusability[InstrumentConfigAndItcResult]       = Reusability.byEq
  given Reusability[GuideStarCandidate]                 = Reusability.by(_.name.value)
  given Reusability[AgsPosition]                        = Reusability.byEq
  given Reusability[AgsParams]                          = Reusability.byEq
  given Reusability[AgsState]                           = Reusability.byEq
  given Reusability[ObsConfiguration]                   = Reusability.byEq
  given Reusability[ConfigurationForVisualization]      = Reusability.byEq
  given Reusability[Existence]                          = Reusability.byEq
  given Reusability[ItcExposureTime]                    = Reusability.byEq
  given Reusability[ItcInstrumentConfig]                = Reusability.byEq
  given Reusability[ConfigSelection]                    = Reusability.byEq
  given Reusability[CentralWavelength]                  = Reusability.byEq
  given Reusability[Tracking]                           = Reusability.byEq
  given Reusability[ObservationTargets]                 = Reusability.byEq
  given Reusability[ObservationTargetsCoordinatesAt]    = Reusability.byEq
  given Reusability[RegionOrTrackingMap]                = Reusability.byEq
  given Reusability[TargetWithOptId]                    = Reusability.byEq
  given Reusability[GlobalPreferences]                  = Reusability.byEq
  given Reusability[PFVisibility]                       = Reusability.byEq
  given Reusability[SelectedPanel]                      = Reusability.byEq
  given Reusability[Configuration]                      = Reusability.byEq
  given Reusability[ConfigurationRequest]               = Reusability.byEq
  given Reusability[TimingWindow]                       = Reusability.byEq
  given [D]: Reusability[Visit[D]]                      = Reusability.byEq
  given [D]: Reusability[StepRecord[D]]                 = Reusability.byEq
  given Reusability[ApiKey]                             = Reusability.byEq
  given Reusability[SignalToNoise]                      = Reusability.byEq
  given Reusability[ScienceRequirements.Spectroscopy]   = Reusability.byEq
  given Reusability[ScienceRequirements.Imaging]        = Reusability.byEq
  given Reusability[ScienceRequirements]                = Reusability.byEq
  given Reusability[Transformation]                     = Reusability.byEq
  given [F[_]]: Reusability[OdbRestClient[F]]           = Reusability.by(_.authToken)
  given [D: Eq]: Reusability[Atom[D]]                   = Reusability.byEq
  given Reusability[ExecutionVisits]                    = Reusability.byEq
  given Reusability[ProgramNote]                        = Reusability.byEq
  given Reusability[ProgramUser]                        = Reusability.byEq
  given Reusability[UserInvitation]                     = Reusability.byEq
  given Reusability[IsActive]                           = Reusability.byEq
  given Reusability[PAProperties]                       = Reusability.byEq
  given Reusability[GraphResult]                        = Reusability.byEq
  given Reusability[ItcCcd]                             = Reusability.byEq
  given Reusability[PartnerSplit]                       = Reusability.byEq
  given Reusability[CallForProposal]                    = Reusability.byEq
  given Reusability[CategoryAllocationList]             = Reusability.byEq
  given Reusability[InstrumentOverrides]                = Reusability.byEq
  given Reusability[SignalToNoiseModeInfo]              = Reusability.byEq
  given Reusability[TimeAndCountModeInfo]               = Reusability.byEq
  given [A: Reusability]: Reusability[NonEmptyChain[A]] = Reusability.by(_.toNonEmptyList)
  given Reusability[WavelengthDither]                   = Reusability.byEq
  given Reusability[ExposureTimeMode]                   = Reusability.byEq
  given [A]: Reusability[Offset.Component[A]]           = Reusability.byEq
  // We explicitly leave default binning out of ObservingMode Reusability since we compute it each time, ignoring the server value.
  given Reusability[ObservingMode.GmosNorthLongSlit]    =
    Reusability.by: x =>
      (x.grating,
       x.filter,
       x.fpu,
       x.centralWavelength,
       x.explicitXBin,
       x.explicitYBin,
       x.ampReadMode,
       x.ampGain,
       x.roi,
       x.wavelengthDithers,
       x.spatialOffsets,
       x.exposureTimeMode,
       x.acquisition.filter,
       x.acquisition.exposureTimeMode
      )
  given Reusability[ObservingMode.GmosSouthLongSlit]    =
    Reusability.by: x =>
      (x.grating,
       x.filter,
       x.fpu,
       x.centralWavelength,
       x.explicitXBin,
       x.explicitYBin,
       x.ampReadMode,
       x.ampGain,
       x.roi,
       x.wavelengthDithers,
       x.spatialOffsets,
       x.exposureTimeMode,
       x.acquisition.filter,
       x.acquisition.exposureTimeMode
      )
  given Reusability[ObservingMode.GmosNorthImaging]     = Reusability.byEq
  given Reusability[ObservingMode.GmosSouthImaging]     = Reusability.byEq
  given Reusability[ObservingMode.Flamingos2LongSlit]   = Reusability.byEq

  given reuseGmosNorthImagingFilter: Reusability[ObservingMode.GmosNorthImaging.ImagingFilter] =
    Reusability.byEq
  given reuseGmosSouthImagingFilter: Reusability[ObservingMode.GmosSouthImaging.ImagingFilter] =
    Reusability.byEq

  given Reusability[ObservingMode] = Reusability:
    case (x: ObservingMode.GmosNorthLongSlit, y: ObservingMode.GmosNorthLongSlit)   =>
      summon[Reusability[ObservingMode.GmosNorthLongSlit]].test(x, y)
    case (x: ObservingMode.GmosSouthLongSlit, y: ObservingMode.GmosSouthLongSlit)   =>
      summon[Reusability[ObservingMode.GmosSouthLongSlit]].test(x, y)
    case (x: ObservingMode.GmosNorthImaging, y: ObservingMode.GmosNorthImaging)     =>
      summon[Reusability[ObservingMode.GmosNorthImaging]].test(x, y)
    case (x: ObservingMode.GmosSouthImaging, y: ObservingMode.GmosSouthImaging)     =>
      summon[Reusability[ObservingMode.GmosSouthImaging]].test(x, y)
    case (x: ObservingMode.Flamingos2LongSlit, y: ObservingMode.Flamingos2LongSlit) =>
      summon[Reusability[ObservingMode.Flamingos2LongSlit]].test(x, y)
    case _                                                                          => false

  // Since we extend the hierarchy here, we need to provide this instance manually
  given [D: Eq]: Reusability[SequenceRow[D]] = Reusability:
    case (a: SequenceRow.FutureStep[D], b: SequenceRow.FutureStep[D])                         => a === b
    case (a: SequenceRow.Executed.ExecutedStep[D], b: SequenceRow.Executed.ExecutedStep[D])   =>
      a === b
    case (a: SequenceRow.Executed.ExecutedVisit[D], b: SequenceRow.Executed.ExecutedVisit[D]) =>
      a === b
    case (_, _)                                                                               => false
