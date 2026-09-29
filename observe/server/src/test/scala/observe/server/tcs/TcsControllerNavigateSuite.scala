// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.tcs

import cats.data.NonEmptySet
import cats.syntax.all.*
import clue.data.Ignore
import clue.data.syntax.*
import coulomb.Quantity
import coulomb.syntax.*
import coulomb.units.accepted.ArcSecond
import coulomb.units.accepted.Millimeter
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.Site
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.schemas.NavigateDB.Types.ConfigureStepInput
import lucuma.schemas.NavigateDB.Types.DistanceInput
import lucuma.schemas.NavigateDB.Types.LightPathInput
import lucuma.schemas.ObservationDB.Types.OffsetComponentInput
import lucuma.schemas.ObservationDB.Types.OffsetInput
import lucuma.schemas.ObservationDB.Types.WavelengthInput
import lucuma.schemas.model.navigate.LightSinkVariant
import lucuma.schemas.model.navigate.LightSource
import observe.common.NavigateQueriesGQL.ConfigureStepMutation
import observe.server.InstrumentGuide
import observe.server.Length
import observe.server.tcs.TcsController.*
import observe.server.tcs.TcsController.Subsystem.*

class TcsControllerNavigateSuite extends munit.FunSuite {

  private val gmosNorth: InstrumentGuide = new InstrumentGuide {
    override def instrument: Instrument                                       = Instrument.GmosNorth
    override def oiOffsetGuideThreshold: Option[Quantity[Double, Millimeter]] = none
  }

  private val guidingOn: StepGuideState  = StepGuideState.Enabled
  private val guidingOff: StepGuideState = StepGuideState.Disabled

  private val offset: InstrumentOffset =
    InstrumentOffset(OffsetP(1.5.withUnit[ArcSecond]), OffsetQ((-2.25).withUnit[ArcSecond]))

  private val wavelength: Wavelength = Wavelength.fromIntNanometers(650).get

  private val defocus: Length = Length.fromLongMicrometers(-120)

  private def tcsConfig(
    guiding:   StepGuideState,
    lightPath: LightPath = LightPath(LightSource.Sky, LightSinkName.Gmos)
  ): BasicTcsConfig[Site.GN.type] =
    BasicTcsConfig[Site.GN.type](
      TelescopeConfig(offset.some, wavelength.some, defocus.some),
      AGConfig(lightPath, HrwfsConfig.Auto.some),
      gmosNorth,
      guiding
    )

  // Offset components are sent as unsigned microarcseconds, as the ODB input helpers encode them:
  // -2.25" is sent as 360° - 2.25".
  private val expectedP: Long = 1500000L
  private val expectedQ: Long = Angle.µasPer360 - 2250000L

  private val expectedOffset: OffsetInput = OffsetInput(
    OffsetComponentInput.Microarcseconds(expectedP),
    OffsetComponentInput.Microarcseconds(expectedQ)
  )

  test("Offset conversion to the core Offset keeps the instrument offset") {
    val converted = offset.toOffset.toInstrumentOffset
    assertEqualsDouble(converted.p.value.value, offset.p.value.value, 1e-6)
    assertEqualsDouble(converted.q.value.value, offset.q.value.value, 1e-6)
  }

  test("Science step sets offset, wavelength, light path and defocus, and guides if the step does") {
    assertEquals(
      TcsControllerNavigate.configureStepInput(Subsystem.allButGaos, tcsConfig(guidingOn)),
      ConfigureStepInput(
        offset = expectedOffset.assign,
        wavelength = WavelengthInput.Picometers(wavelength.toPicometers.value).assign,
        lightPath = LightPathInput(LightSource.Sky, Instrument.GmosNorth).assign,
        defocus = DistanceInput.Micrometers(-120L).assign,
        guiding = true
      )
    )
  }

  test("Step does not guide if its guiding is disabled") {
    assert(
      !TcsControllerNavigate.configureStepInput(Subsystem.allButGaos, tcsConfig(guidingOff)).guiding
    )
  }

  test("Guiding follows the step's guide state whichever subsystems are configured") {
    assert(
      TcsControllerNavigate
        .configureStepInput(NonEmptySet.of(AGUnit), tcsConfig(guidingOn))
        .guiding
    )
  }

  test("Offset and wavelength are only set when the mount is configured") {
    val input = TcsControllerNavigate.configureStepInput(
      NonEmptySet.of(AGUnit, OIWFS),
      tcsConfig(guidingOff, LightPath(LightSource.GCAL, LightSinkName.Gmos))
    )
    assertEquals(input.offset, Ignore)
    assertEquals(input.wavelength, Ignore)
    assertEquals(
      input.lightPath,
      LightPathInput(LightSource.GCAL, Instrument.GmosNorth).assign
    )
  }

  test("Defocus is only set when M2 is configured") {
    assertEquals(
      TcsControllerNavigate
        .configureStepInput(NonEmptySet.of(AGUnit, OIWFS), tcsConfig(guidingOn))
        .defocus,
      Ignore
    )
  }

  test("Light path is only set when the A&G unit is configured") {
    assertEquals(
      TcsControllerNavigate
        .configureStepInput(NonEmptySet.of(Mount, M1, M2), tcsConfig(guidingOn))
        .lightPath,
      Ignore
    )
  }

  test("Light sink variants are sent for the IFU and NIRI cameras") {
    assertEquals(
      TcsControllerNavigate.lightPathInput(
        LightPath(LightSource.Sky, LightSinkName.Gmos_Ifu),
        Instrument.GmosNorth
      ),
      LightPathInput(LightSource.Sky, Instrument.GmosNorth, LightSinkVariant.GmosIfu.assign)
    )
    assertEquals(
      TcsControllerNavigate
        .lightPathInput(LightPath(LightSource.AO, LightSinkName.Niri_f32), Instrument.Niri),
      LightPathInput(LightSource.AO, Instrument.Niri, LightSinkVariant.NiriF32.assign)
    )
  }

  test("Nod only moves to the nod offset") {
    val nodOffset =
      InstrumentOffset(OffsetP(0.0.withUnit[ArcSecond]), OffsetQ(10.0.withUnit[ArcSecond]))
    val input     =
      TcsControllerNavigate.nodInput(Subsystem.allButGaos, nodOffset, true)
    assertEquals(
      input,
      ConfigureStepInput(
        offset = OffsetInput(
          OffsetComponentInput.Microarcseconds(0L),
          OffsetComponentInput.Microarcseconds(10000000L)
        ).assign,
        guiding = true
      )
    )
  }

  test("Nod to an unguided position does not guide") {
    assert(
      !TcsControllerNavigate
        .nodInput(Subsystem.allButGaos, offset, false)
        .guiding
    )
  }

  test("configureStep variables are encoded as Navigate expects") {
    assertEquals(
      ConfigureStepMutation.varEncoder(
        ConfigureStepMutation.Variables(
          TcsControllerNavigate.configureStepInput(
            Subsystem.allButGaos,
            tcsConfig(guidingOn, LightPath(LightSource.Sky, LightSinkName.Gmos_Ifu))
          )
        )
      ),
      Json.obj(
        "config" -> Json.obj(
          "offset"     -> Json.obj(
            "p" -> Json.obj("microarcseconds" -> expectedP.asJson),
            "q" -> Json.obj("microarcseconds" -> expectedQ.asJson)
          ),
          "wavelength" -> Json.obj("picometers" -> 650000.asJson),
          "lightPath"  -> Json.obj(
            "from"             -> "SKY".asJson,
            "instrument"       -> "GMOS_NORTH".asJson,
            "lightSinkVariant" -> "GMOS_IFU".asJson
          ),
          "defocus"    -> Json.obj("micrometers" -> -120L.asJson),
          "guiding"    -> true.asJson
        )
      )
    )
  }
}
