// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import cats.effect.IO
import cats.syntax.option.*
import crystal.react.View
import explore.config.sequence.SequenceTable
import explore.config.sequence.SequenceTableBuilder
import japgolly.scalajs.react.callback.Callback
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.*
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.sequence.IsEditEnabled
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.byInstrument.SpectroscopySequenceTable

final case class GhostSequenceTable(
  visits:               View[Option[ExecutionVisits]],
  staticConfig:         GhostStaticConfig,
  science:              View[List[Atom[GhostDynamicConfig]]],
  isEditingAcquisition: View[IsEditing],
  isEditingScience:     View[IsEditing],
  isUserStaffOrAdmin:   Boolean,
  remoteReplace:        SequenceType => List[Atom[GhostDynamicConfig]] => IO[
    List[Atom[GhostDynamicConfig]]
  ]
) extends ReactFnProps(GhostSequenceTable.component)
    with SequenceTable[GhostStaticConfig, GhostDynamicConfig]
    with SpectroscopySequenceTable[GhostDynamicConfig]:

  // No acquisition for GHOST. It is handled internally.
  override val acquisition  = View(List.empty, (_, _) => Callback.empty)
  override val acquisitonSN = none

  // GHOST ITC reports red/blue separately
  override val scienceSN    = none

  // No ghost editing yet
  override val isEditEnabled = IsEditEnabled.False

  override val toInstrumentVisits =
    case ExecutionVisits.Ghost(visits) => visits

object GhostSequenceTable
    extends SequenceTableBuilder[GhostStaticConfig, GhostDynamicConfig](Instrument.Ghost)
