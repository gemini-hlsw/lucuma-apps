// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.Timestamp
import observe.server.TestCommon.*
import observe.server.gnirs.Gnirs
import observe.server.odb.OdbObservationData

class GnirsSuite extends TestCommon {

  private val gnirsStaticCfg: GnirsStaticConfig = GnirsStaticConfig(GnirsWellDepth.Deep)

  // Acquisition-mirror In => imaging/acquisition, no grating/prism (mode-independent passthrough).
  private val gnirsDynamicCfg: GnirsDynamicConfig = GnirsDynamicConfig(
    exposure = 5.secondTimeSpan,
    coadds = PosInt.unsafeFrom(2),
    filter = GnirsFilter.K,
    decker = GnirsDecker.LongCamLongSlit,
    fpu = GnirsFpuSlit.LongSlit_0_30.asLeft,
    acquisitionMirror = GnirsAcquisitionMirrorMode.In,
    camera = GnirsCamera.LongBlue,
    focus = GnirsFocus.Best,
    readMode = GnirsReadMode.Faint
  )

  private def gnirsOdbData: OdbObservationData = {
    val step: Step[GnirsDynamicConfig] =
      Step(
        stepId1,
        gnirsDynamicCfg,
        StepConfig.Science,
        telescopeCfg1,
        StepEstimate.Zero,
        ObserveClass.Science,
        Breakpoint.Disabled
      )
    val atom: Atom[GnirsDynamicConfig]            = Atom(atomId1, none, NonEmptyList.one(step))
    OdbObservationData(
      // nextStep requires an observation time to be set.
      odbObservation(seqObsId1).copy(observationTime = Timestamp.Min.some),
      InstrumentExecutionConfig.Gnirs(
        ExecutionConfig(gnirsStaticCfg, ExecutionSequence(atom, List.empty, false).some, none)
      )
    )
  }

  test("SeqTranslate builds a GNIRS step from a GNIRS execution config") {
    for {
      systems <- defaultSystems
      sg      <- generateStepGen[IO](gnirsOdbData, systems)
    } yield {
      assert(sg.exists(_.isInstanceOf[StepGen.Gnirs[IO]]), "expected a StepGen.Gnirs")
      assert(sg.exists(_.resources.contains(Instrument.Gnirs)), "expected GNIRS in resources")
    }
  }

  test("GNIRS calcObserveTime accounts for coadds and read-mode readout") {
    // (5s exposure + 11.4s readout/coadd) * 2 coadds = 32.8s
    assertEquals(
      Gnirs.calcObserveTime(gnirsDynamicCfg).toMicroseconds,
      32_800_000L
    )
  }
}
