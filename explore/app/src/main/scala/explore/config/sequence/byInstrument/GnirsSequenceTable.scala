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
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.sequence.IsEditEnabled
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.byInstrument.SpectroscopySequenceTable

final case class GnirsSequenceTable(
  visits:               View[Option[ExecutionVisits]],
  staticConfig:         GnirsStaticConfig,
  science:              View[List[Atom[GnirsDynamicConfig]]],
  isEditingAcquisition: View[IsEditing],
  isEditingScience:     View[IsEditing],
  isUserStaffOrAdmin:   Boolean,
  remoteReplace:        SequenceType => List[Atom[GnirsDynamicConfig]] => IO[
    List[Atom[GnirsDynamicConfig]]
  ]
) extends ReactFnProps(GnirsSequenceTable.component)
    with SequenceTable[GnirsStaticConfig, GnirsDynamicConfig]
    with SpectroscopySequenceTable[GnirsDynamicConfig]:

  override val acquisition  = View(List.empty, (_, _) => Callback.empty)
  override val acquisitonSN = none
  override val scienceSN    = none

  override val isEditEnabled = IsEditEnabled.False

  override val toInstrumentVisits =
    case ExecutionVisits.Gnirs(visits) => visits

object GnirsSequenceTable
    extends SequenceTableBuilder[GnirsStaticConfig, GnirsDynamicConfig](Instrument.Gnirs)
