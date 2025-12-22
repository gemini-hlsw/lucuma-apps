// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.syntax.all.*
import explore.Icons
import explore.model.ErrorMsgOr
import explore.model.Group
import explore.model.Observation
import explore.model.ObservationTargets
import explore.model.RegionOrCoordinatesAt
import explore.syntax.ui.*
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Target
import lucuma.schemas.model.TargetWithId

// 24 October 2024 - scalafix failing to parse with fewer braces
// Helper ADT for table rows type
enum ObsSummaryRow:
  val obs: Observation
  val coordsOrRegion: Option[ErrorMsgOr[RegionOrCoordinatesAt]]

  case ExpandedTargetRow(
    obs:            Observation,
    targetWithId:   TargetWithId,
    coordsOrRegion: Option[ErrorMsgOr[RegionOrCoordinatesAt]]
  ) extends ObsSummaryRow

  case ObsRow(
    obs:            Observation,
    targetWithId:   Option[TargetWithId],
    asterism:       Option[ObservationTargets],
    coordsOrRegion: Option[ErrorMsgOr[RegionOrCoordinatesAt]],
    group:          Option[Group]
  ) extends ObsSummaryRow

  def fold[A](f: ExpandedTargetRow => A, g: ObsRow => A): A =
    this match
      case r: ExpandedTargetRow => f(r)
      case r: ObsRow            => g(r)

  def isLastAsterismTargetOf: Option[Observation.Id] = fold(
    targetRow =>
      Option.when(obs.scienceTargetIds.lastOption.contains_(targetRow.targetWithId.id))(obs.id),
    _ => none
  )

  def iconWithTooltip: Option[VdomNode] = fold(
    e => e.targetWithId.target.iconWithTooltip.some,
    o =>
      o.asterism.map(a =>
        if (a.isMixed) Icons.Stars.fixedWidthWithTooltip("Mixed Asterism")
        else a.focus.target.iconWithTooltip
      )
  )
