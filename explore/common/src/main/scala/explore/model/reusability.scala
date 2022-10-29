// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import clue.PersistentClientStatus
import explore.data.KeyedIndexedList
import explore.events.CatalogMessage
import explore.model.Asterism
import explore.model.ObjectTracking
import explore.model.enums.SelectedPanel
import explore.model.itc.CoverageCenterWavelength
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcTarget
import explore.modes.InstrumentRow
import explore.undo.UndoStacks
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.ags.AgsParams
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.catalog.AngularSize
import lucuma.catalog.CatalogTargetResult
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.IntPercent
import lucuma.core.model.Partner
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Proposal
import lucuma.core.model.ProposalClass
import lucuma.core.model.Target
import lucuma.core.util.NewType
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.ui.reusability.*
import queries.schemas.odb.ObsQueries.ObsSummariesWithConstraints
import queries.schemas.odb.ObsQueries.SpectroscopyRequirementsData

import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeSeqMap

/**
 * Reusability instances for model classes
 */
object reusability:
  // Model
  implicit val itcTargetProps: Reusability[ItcTarget]                                = Reusability.byEq
  implicit val statusReuse: Reusability[PersistentClientStatus]                      = Reusability.byEq
  implicit val targetOptionsReuse: Reusability[TargetVisualOptions]                  = Reusability.byEq
  implicit val userVaultReuse: Reusability[UserVault]                                = Reusability.byEq
  implicit val targetViewExpandedIdsReuse: Reusability[ExpandedIds]                  = Reusability.byEq
  implicit val rootModelReuse: Reusability[RootModel]                                = Reusability.byEq
  implicit def idListReuse[Id, A: Reusability]: Reusability[KeyedIndexedList[Id, A]] =
    Reusability.by(_.toList)

  implicit val scienceTargetsReuse: Reusability[TreeSeqMap[Target.Id, Target]]        =
    Reusability.by((_: TreeSeqMap[Target.Id, Target]).toMap)(Reusability.map)
  implicit val obsIdSetReuse: Reusability[ObsIdSet]                                   = Reusability.byEq
  implicit val targetIdSetReuse: Reusability[TargetIdSet]                             = Reusability.byEq
  implicit val targetWithIdReuse: Reusability[TargetWithId]                           = Reusability.byEq
  implicit val targetWithIdAndObsReuse: Reusability[TargetWithIdAndObs]               = Reusability.byEq
  implicit val targetWithObsReuse: Reusability[TargetWithObs]                         = Reusability.byEq
  implicit val constraintsSummaryReuse: Reusability[ConstraintsSummary]               = Reusability.byEq
  implicit val constraintGroupReuse: Reusability[ConstraintGroup]                     = Reusability.byEq
  implicit val obsSummaryReuse: Reusability[ObsSummary]                               = Reusability.byEq
  implicit val localPreferencesReuse: Reusability[ExploreLocalPreferences]            = Reusability.byEq
  implicit val posAngleReuse: Reusability[PosAngleConstraint]                         = Reusability.byEq
  implicit val obsSummaryWithConstraintsReuse: Reusability[ObsSummaryWithConstraints] =
    Reusability.byEq
  implicit val obsSummaryWithTargetsAndConstraintsReuse
    : Reusability[ObsSummaryWithTitleAndConstraints] = Reusability.byEq
  implicit val obsSummaryWithTargetsConstraintsAndConfReuse
    : Reusability[ObsSummaryWithTitleConstraintsAndConf] = Reusability.byEq

  // Undo
  implicit def undoStacksReuse[F[_], M]: Reusability[UndoStacks[F, M]]               =
    Reusability.by(s => (s.undo.length, s.redo.length, s.working))
  implicit def undoStacksMapReuse[F[_], K, M]: Reusability[Map[K, UndoStacks[F, M]]] =
    Reusability.by[Map[K, UndoStacks[F, M]], Int](_.size) && Reusability[Map[K, UndoStacks[F, M]]](
      (a, b) =>
        a.forall { case (k, stacksA) =>
          b.get(k).exists(stacksB => undoStacksReuse.test(stacksA, stacksB))
        }
    )
  implicit def modelUndoStacksReuse[F[_]]: Reusability[ModelUndoStacks[F]]           = Reusability.byEq

  implicit val filterReuse: Reusability[AvailableFilter]              = Reusability.byEq
  implicit val optionsReuse: Reusability[ImagingConfigurationOptions] = Reusability.byEq
  implicit val percentageReuse: Reusability[Progress]                 = Reusability.byEq

  implicit val angularSizeReuse: Reusability[AngularSize]                                   = Reusability.byEq
  implicit val catalogTargetReuse: Reusability[CatalogTargetResult]                         = Reusability.byEq
  implicit val scienceModeBasicReuse: Reusability[ScienceModeBasic]                         = Reusability.byEq
  implicit val scienceModeAdvancedResultReuse: Reusability[ScienceModeAdvanced]             = Reusability.byEq
  implicit val gmosNorthLongSlitBasicReuse: Reusability[ScienceModeBasic.GmosNorthLongSlit] =
    Reusability.byEq
  implicit val gmosSouthLongSlitBasicReuse: Reusability[ScienceModeBasic.GmosSouthLongSlit] =
    Reusability.byEq
  implicit val scienceModeResultReuse: Reusability[ScienceMode]                             = Reusability.byEq
  implicit val guideStarReuse: Reusability[GuideStarCandidate]                              = Reusability.by(_.name.value)
  implicit val agsPositionReuse: Reusability[AgsPosition]                                   = Reusability.byEq
  implicit val agsParamsReuse: Reusability[AgsParams]                                       = Reusability.byEq

  implicit val partnerSplitsReuse: Reusability[SortedMap[Partner, IntPercent]] =
    Reusability.by((_: SortedMap[Partner, IntPercent]).toMap)(Reusability.map)
  implicit val proposalClassReuse: Reusability[ProposalClass]                  = Reusability.byEq
  implicit val proposalReuse: Reusability[Proposal]                            = Reusability.byEq
  implicit val obsConfReuse: Reusability[ObsConfiguration]                     = Reusability.byEq

  implicit val existenceReuse: Reusability[Existence]                       = Reusability.byEq
  implicit val requirementsReuse: Reusability[SpectroscopyRequirementsData] = Reusability.byEq

  given Reusability[ItcChartExposureTime] = Reusability.byEq

  given Reusability[InstrumentRow] = Reusability.byEq

  given Reusability[CoverageCenterWavelength] = Reusability.byEq

  given Reusability[ObjectTracking] = Reusability.byEq

  given reusabilityNewType[W, T <: NewType[W]#Type](using
    reusability: Reusability[W]
  ): Reusability[T] =
    reusability.asInstanceOf[Reusability[T]]

  given Reusability[Asterism] = Reusability.byEq[Asterism]

  given Reusability[TargetWithOptId] = Reusability.byEq

  given Reusability[UserGlobalPreferences] = Reusability.byEq

  given Reusability[ObsSummariesWithConstraints] = Reusability.byEq

  given Reusability[Range.Inclusive] = Reusability.by(x => (x.start, x.end, x.step))

  given Reusability[SelectedPanel] = Reusability.byEq
