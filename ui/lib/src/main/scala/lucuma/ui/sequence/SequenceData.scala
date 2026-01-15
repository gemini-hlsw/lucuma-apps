// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.schemas.odb.SequenceQueriesGQL.SequenceQuery
import monocle.Focus
import monocle.Lens

import SequenceQuery.Data.Observation.Itc

/**
 * Bundles the execution configuration and the signal-to-noise ratio for each sequence type.
 */
final case class SequenceData(
  config:        InstrumentExecutionConfig,
  signalToNoise: InstrumentSignalToNoise
) derives Eq

object SequenceData:
  private def buildSnMap(itc: Itc): InstrumentSignalToNoise =
    itc match
      case Itc.ItcSpectroscopy(acquisition, science) =>
        InstrumentSignalToNoise.Spectroscopy(
          (acquisition.selected.signalToNoiseAt.map(sn => SingleSN(sn.single)),
           acquisition.selected.signalToNoiseAt.map(sn => TotalSN(sn.total))
          ).tupled,
          (science.selected.signalToNoiseAt.map(sn => SingleSN(sn.single)),
           science.selected.signalToNoiseAt.map(sn => TotalSN(sn.total))
          ).tupled
        )
      case Itc.ItcGmosNorthImaging(results)          =>
        InstrumentSignalToNoise.GmosNorthImaging:
          results
            .flatMap: result =>
              result.results.selected.signalToNoiseAt.map: sn =>
                (result.filter, (SingleSN(sn.single), TotalSN(sn.total)))
            .toMap
      case Itc.ItcGmosSouthImaging(results)          =>
        InstrumentSignalToNoise.GmosSouthImaging:
          results
            .flatMap: result =>
              result.results.selected.signalToNoiseAt.map: sn =>
                (result.filter, (SingleSN(sn.single), TotalSN(sn.total)))
            .toMap

  def fromOdbResponse(data: SequenceQuery.Data): Option[SequenceData] =
    data.executionConfig.map: config =>
      val sn: InstrumentSignalToNoise = data.observation
        .map(obs => buildSnMap(obs.itc))
        .getOrElse(InstrumentSignalToNoise.Undefined)
      SequenceData(config, sn)

  val config: Lens[SequenceData, InstrumentExecutionConfig]      = Focus[SequenceData](_.config)
  val signalToNoise: Lens[SequenceData, InstrumentSignalToNoise] =
    Focus[SequenceData](_.signalToNoise)

  given Reusability[SequenceData] = Reusability.byEq
