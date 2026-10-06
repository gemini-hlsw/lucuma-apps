// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s.input

import cats.syntax.all.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.OffsetInput
import lucuma.odb.graphql.input.WavelengthInput
import lucuma.schemas.model.navigate.LightSinkVariant
import navigate.model.Distance
import navigate.model.LightPath
import navigate.model.enums.LightSink

object LightPathInput:

  def lightSink(
    rInstrument: Result[Instrument],
    rVariant:    Result[Option[LightSinkVariant]]
  ): Result[LightSink] =
    (rInstrument, rVariant).parTupled.flatMap: (instrument, variant) =>
      LightSink
        .fromInstrumentAndVariant(instrument, variant)
        .toResult(
          s"No light sink for instrument $instrument${variant.foldMap(v => s" with variant $v")}."
        )

  val Binding: Matcher[LightPath] =
    ObjectFieldsBinding.rmap:
      case List(
            LightSourceBinding("from", rFrom),
            InstrumentBinding("instrument", rInstrument),
            LightSinkVariantBinding.Option("lightSinkVariant", rLightSinkVariant)
          ) =>
        (rFrom, lightSink(rInstrument, rLightSinkVariant)).parMapN(LightPath.apply)

final case class ConfigureStepInput(
  offset:     Option[Offset],
  wavelength: Option[Wavelength],
  lightPath:  Option[LightPath],
  defocus:    Option[Distance],
  guiding:    Boolean
)

object ConfigureStepInput:

  val Binding: Matcher[ConfigureStepInput] =
    ObjectFieldsBinding.rmap:
      case List(
            OffsetInput.Binding.Option("offset", rOffset),
            WavelengthInput.Binding.Option("wavelength", rWavelength),
            LightPathInput.Binding.Option("lightPath", rLightPath),
            DistanceInput.Binding.Option("defocus", rDefocus),
            BooleanBinding("guiding", rGuiding)
          ) =>
        (rOffset, rWavelength, rLightPath, rDefocus, rGuiding).parMapN(ConfigureStepInput.apply)
