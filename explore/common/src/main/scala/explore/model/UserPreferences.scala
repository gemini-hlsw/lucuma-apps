// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.derived.*
import cats.implicits.*
import explore.model.enums.GridLayoutSection
import explore.model.enums.LineOfSightMotion
import explore.model.layout.LayoutsMap
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.util.NewType
import monocle.Focus
import monocle.Lens
import monocle.function.At.*
import monocle.function.At.given

case class UserPreferences(
  private val gridLayouts: Map[GridLayoutSection, LayoutsMap],
  globalPreferences:       GlobalPreferences,
  targetPreferences:       Map[Target.Id, LineOfSightMotion] = Map.empty,
  observationPreferences:  Map[Observation.Id, Target.Id] = Map.empty,
  asterismPreferences:     Map[UserPreferences.AsterismKey, AsterismVisualOptions] = Map.empty
) derives Eq {
  private def tabLayout(l: GridLayoutSection) =
    gridLayouts.getOrElse(l, ExploreGridLayouts.sectionLayout(l))

  val constraintsTabLayout =
    tabLayout(GridLayoutSection.ConstraintsLayout)

  val targetTabLayout =
    tabLayout(GridLayoutSection.TargetLayout)

  val schedulingTabLayout =
    tabLayout(GridLayoutSection.SchedulingLayout)

  val observationsTabLayout =
    tabLayout(GridLayoutSection.ObservationsLayout)

  val specPhotoTabLayout =
    tabLayout(GridLayoutSection.ObservationsSpecPhotoLayout)

  val twilightTabLayout =
    tabLayout(GridLayoutSection.ObservationsTwilightLayout)

  val sequenceTileLayout =
    tabLayout(GridLayoutSection.ObservationsSequenceLayout)

  val observationListTabLayout =
    tabLayout(GridLayoutSection.ObservationListLayout)

  val programsTabLayout =
    tabLayout(GridLayoutSection.ProgramsLayout)

  val overviewTabLayout =
    tabLayout(GridLayoutSection.OverviewLayout)

  val proposalTabLayout =
    tabLayout(GridLayoutSection.ProposalLayout)

  val groupEditLayout =
    tabLayout(GridLayoutSection.GroupEditLayout)
}

object UserPreferences:
  object AsterismKey extends NewType[NonEmptySet[Target.Id]]:
    def fromTargetIds(tids: NonEmptyList[Target.Id]): AsterismKey =
      AsterismKey(tids.toNes)
  type AsterismKey = AsterismKey.Type

  val Default =
    UserPreferences(ExploreGridLayouts.DefaultLayouts,
                    GlobalPreferences.Default,
                    Map.empty,
                    Map.empty,
                    Map.empty
    )

  val gridLayouts            = Focus[UserPreferences](_.gridLayouts)
  val globalPreferences      = Focus[UserPreferences](_.globalPreferences)
  val targetPreferences      = Focus[UserPreferences](_.targetPreferences)
  val observationPreferences = Focus[UserPreferences](_.observationPreferences)
  val asterismPreferences    = Focus[UserPreferences](_.asterismPreferences)

  def asterismVisualOptions(
    key: AsterismKey
  ): Lens[UserPreferences, Option[AsterismVisualOptions]] =
    UserPreferences.asterismPreferences
      .andThen(
        at[Map[AsterismKey, AsterismVisualOptions], AsterismKey, Option[AsterismVisualOptions]](key)
      )

  def targetLineOfSightMotion(tid: Target.Id): Lens[UserPreferences, Option[LineOfSightMotion]] =
    UserPreferences.targetPreferences
      .andThen(
        at[Map[Target.Id, LineOfSightMotion], Target.Id, Option[LineOfSightMotion]](tid)
      )

  def observationPreferredTarget(oid: Observation.Id): Lens[UserPreferences, Option[Target.Id]] =
    UserPreferences.observationPreferences
      .andThen(
        at[Map[Observation.Id, Target.Id], Observation.Id, Option[Target.Id]](oid)
      )
