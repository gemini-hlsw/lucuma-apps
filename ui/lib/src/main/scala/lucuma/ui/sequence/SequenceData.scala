// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Eq
import cats.derived.*
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.schemas.model.ModeSignalToNoise
import lucuma.schemas.odb.SequenceQueriesGQL.SequenceQuery
import monocle.Focus
import monocle.Lens

/**
 * Bundles the execution configuration and the signal-to-noise ratio for each sequence type.
 */
final case class SequenceData(
  config:        InstrumentExecutionConfig,
  signalToNoise: ModeSignalToNoise
) derives Eq

object SequenceData:
  def fromOdbResponse(data: SequenceQuery.Data): Option[SequenceData] =
    data.executionConfig.map: config =>
      SequenceData(
        config,
        data.observation.map(_.signalToNoise).getOrElse(ModeSignalToNoise.Undefined)
      )

  val config: Lens[SequenceData, InstrumentExecutionConfig] = Focus[SequenceData](_.config)
  val signalToNoise: Lens[SequenceData, ModeSignalToNoise]  =
    Focus[SequenceData](_.signalToNoise)

  given Reusability[SequenceData] = Reusability.byEq
