// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.effect.Ref
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.enums.Instrument
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.core.refined.auto.*
import lucuma.schemas.model.ModeSignalToNoise
import monocle.Lens
import monocle.syntax.all.focus
import observe.common.EventsGQL.RecordDatasetMutation.Data.RecordDataset.Dataset
import observe.common.ObsQueriesGql.ObsQuery.Data
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation as ODBObservation
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment.GuideEnvironment
import observe.model.dhs.ImageFileId
import observe.model.odb.ObsRecordedIds

trait TestOdbProxy[F[_]] extends OdbProxy[F]:
  def outCapture: F[List[TestOdbProxy.OdbEvent]]

object TestOdbProxy {

  case class SequenceState(
    acquisition: Option[Atom[?]],
    science:     List[Atom[?]]
  ):
    private def completeStepInAtom(
      stepId: Step.Id
    )(a: Atom[?]): Option[Atom[?]] =
      NonEmptyList
        .fromList(a.steps.filterNot(_.id === stepId))
        .map: newSteps =>
          a.copy(steps = newSteps)

    def completeStep(stepId: Step.Id): SequenceState =
      copy(
        acquisition = acquisition.flatMap(completeStepInAtom(stepId)),
        science = science.map(completeStepInAtom(stepId)).flattenOption
      )

    def resetAcquisition(original: Option[Atom[?]]): SequenceState =
      copy(acquisition = original)

  case class InstrumentState(
    obsId:     Observation.Id,
    staticCfg: Any,
    sequence:  SequenceState
  )

  extension (sequences: Map[Instrument, InstrumentState])
    def sequenceForObs(obsId: Observation.Id): Option[(Instrument, SequenceState, Any)] =
      sequences.find(_._2.obsId === obsId).map { case (inst, state) =>
        (inst, state.sequence, state.staticCfg)
      }

    def updateObs(
      obsId: Observation.Id
    )(f: SequenceState => SequenceState): Map[Instrument, InstrumentState] =
      sequenceForObs(obsId)
        .map: (inst, seq, _) =>
          sequences.updated(inst, sequences(inst).copy(sequence = f(seq)))
        .getOrElse(sequences)

  case class State(
    sequences: Map[Instrument, InstrumentState],
    out:       List[OdbEvent]
  ):
    def updateObs(obsId: Observation.Id)(f: SequenceState => SequenceState): State =
      copy(sequences = sequences.updateObs(obsId)(f))

  private def buildInstrumentExecutionSequence(
    i:              Instrument,
    nextAtom:       Atom[?],
    possibleFuture: List[Atom[?]],
    hasMore:        Boolean
  ): ExecutionSequence[?] =
    i match
      case Instrument.GmosNorth  =>
        ExecutionSequence[gmos.DynamicConfig.GmosNorth](
          nextAtom.asInstanceOf[Atom[gmos.DynamicConfig.GmosNorth]],
          possibleFuture.asInstanceOf[List[Atom[gmos.DynamicConfig.GmosNorth]]],
          hasMore
        )
      case Instrument.GmosSouth  =>
        ExecutionSequence[gmos.DynamicConfig.GmosSouth](
          nextAtom.asInstanceOf[Atom[gmos.DynamicConfig.GmosSouth]],
          possibleFuture.asInstanceOf[List[Atom[gmos.DynamicConfig.GmosSouth]]],
          hasMore
        )
      case Instrument.Flamingos2 =>
        ExecutionSequence[Flamingos2DynamicConfig](
          nextAtom.asInstanceOf[Atom[Flamingos2DynamicConfig]],
          possibleFuture.asInstanceOf[List[Atom[Flamingos2DynamicConfig]]],
          hasMore
        )
      case Instrument.Igrins2    =>
        ExecutionSequence[Igrins2DynamicConfig](
          nextAtom.asInstanceOf[Atom[Igrins2DynamicConfig]],
          possibleFuture.asInstanceOf[List[Atom[Igrins2DynamicConfig]]],
          hasMore
        )
      case i                     => sys.error(s"Unexpected instrument $i")

  def buildExecutionConfig(
    i:           Instrument,
    static:      Any,
    acquisition: Option[ExecutionSequence[?]],
    science:     Option[ExecutionSequence[?]]
  ): InstrumentExecutionConfig =
    i match
      case Instrument.GmosNorth  =>
        InstrumentExecutionConfig.GmosNorth(
          ExecutionConfig(
            static.asInstanceOf[gmos.StaticConfig.GmosNorth],
            acquisition.asInstanceOf[Option[ExecutionSequence[gmos.DynamicConfig.GmosNorth]]],
            science.asInstanceOf[Option[ExecutionSequence[gmos.DynamicConfig.GmosNorth]]]
          )
        )
      case Instrument.GmosSouth  =>
        InstrumentExecutionConfig.GmosSouth(
          ExecutionConfig(
            static.asInstanceOf[gmos.StaticConfig.GmosSouth],
            acquisition.asInstanceOf[Option[ExecutionSequence[gmos.DynamicConfig.GmosSouth]]],
            science.asInstanceOf[Option[ExecutionSequence[gmos.DynamicConfig.GmosSouth]]]
          )
        )
      case Instrument.Flamingos2 =>
        InstrumentExecutionConfig.Flamingos2(
          ExecutionConfig(
            static.asInstanceOf[Flamingos2StaticConfig],
            acquisition.asInstanceOf[Option[ExecutionSequence[Flamingos2DynamicConfig]]],
            science.asInstanceOf[Option[ExecutionSequence[Flamingos2DynamicConfig]]]
          )
        )
      case Instrument.Igrins2    =>
        InstrumentExecutionConfig.Igrins2(
          ExecutionConfig(
            static.asInstanceOf[Igrins2StaticConfig],
            acquisition.asInstanceOf[Option[ExecutionSequence[Igrins2DynamicConfig]]],
            science.asInstanceOf[Option[ExecutionSequence[Igrins2DynamicConfig]]]
          )
        )
      case i                     => sys.error(s"Unexpected instrument $i")

  def buildGmosNorth[F[_]: Concurrent](
    obsId:              Observation.Id,
    staticCfg:          gmos.StaticConfig.GmosNorth,
    acquisition:        Option[Atom[gmos.DynamicConfig.GmosNorth]],
    science:            List[Atom[gmos.DynamicConfig.GmosNorth]] = List.empty,
    updateStartObserve: SequenceState => SequenceState = identity
  ): F[TestOdbProxy[F]] =
    build(
      Map(
        Instrument.GmosNorth -> InstrumentState(
          obsId,
          staticCfg,
          SequenceState(acquisition, science)
        )
      ),
      _.updateObs(obsId)(updateStartObserve)
    )

  def build[F[_]: Concurrent](
    sequences:          Map[Instrument, InstrumentState],
    updateStartObserve: State => State = identity
  ): F[TestOdbProxy[F]] =
    Ref
      .of[F, State](State(sequences, List.empty))
      .map(rf =>
        new TestOdbProxy[F] {
          private def addEvent(ev: OdbEvent): F[Unit] =
            rf.modify(s => (s.focus(_.out).modify(_.appended(ev)), ()))

          override def resetAcquisition(obsId: Observation.Id): F[Unit] =
            rf.update:
              _.updateObs(obsId):
                _.resetAcquisition(sequences.sequenceForObs(obsId).flatMap(_._2.acquisition))

          override def read(obsId: Observation.Id): F[OdbObservationData] =
            rf.get
              .map { s =>
                val (i: Instrument, st: SequenceState, staticCfg) =
                  s.sequences
                    .sequenceForObs(obsId)
                    .getOrElse(sys.error(s"Observation $obsId not found in test ODB state"))
                val sciAtom: Option[Atom[?]]                      = st.science.headOption
                val sciTail: List[Atom[?]]                        =
                  st.science match
                    case head :: tail => tail
                    case Nil          => Nil

                OdbObservationData(
                  Data.Observation(
                    obsId,
                    title = "Test Observation".refined,
                    Data.Observation.Program(
                      Program.Id(PosLong.unsafeFrom(1)),
                      None,
                      ODBObservation.Program.Goa(NonNegInt.unsafeFrom(0))
                    ),
                    Data.Observation.TargetEnvironment(none, GuideEnvironment(List.empty)),
                    ConstraintSet(
                      ImageQuality.Preset.TwoPointZero,
                      CloudExtinction.Preset.TwoPointZero,
                      SkyBackground.Bright,
                      WaterVapor.Wet,
                      ElevationRange.ByAirMass.Default
                    ),
                    List.empty,
                    ModeSignalToNoise.Spectroscopy(none, none)
                  ),
                  buildExecutionConfig(
                    i,
                    staticCfg,
                    st.acquisition.map(buildInstrumentExecutionSequence(i, _, List.empty, true)),
                    sciAtom
                      .map(buildInstrumentExecutionSequence(i, _, sciTail, st.science.nonEmpty))
                  )
                )
              }

          override def visitStart[S](obsId: Observation.Id, staticCfg: S): F[Unit] = addEvent(
            VisitStart(obsId, staticCfg)
          )

          override def sequenceStart(obsId: Observation.Id): F[Unit] =
            addEvent(SequenceStart(obsId))

          override def stepStartStep[D](obsId: Observation.Id, stepId: Step.Id): F[Unit] =
            addEvent(StepStartStep(obsId))

          override def stepStartConfigure(obsId: Observation.Id, stepId: Step.Id): F[Unit] =
            addEvent(StepStartConfigure(obsId))

          override def stepEndConfigure(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
            addEvent(StepEndConfigure(obsId)).as(true)

          override def stepStartObserve(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
            addEvent(StepStartObserve(obsId)).as(true)

          override def datasetStartExposure(
            obsId:  Observation.Id,
            stepId: Step.Id,
            fileId: ImageFileId
          ): F[Dataset] =
            addEvent(DatasetStartExposure(obsId, fileId)) *> Dataset(
              lucuma.core.model.sequence.Dataset
                .Id(PosLong.unsafeFrom(scala.util.Random.between(1L, Long.MaxValue))),
              None
            ).pure[F]

          override def datasetEndExposure(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
            addEvent(DatasetEndExposure(obsId, fileId)).as(true)

          override def datasetStartReadout(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
            addEvent(DatasetStartReadout(obsId, fileId)).as(true)

          override def datasetEndReadout(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
            addEvent(DatasetEndReadout(obsId, fileId)).as(true)

          override def datasetStartWrite(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
            addEvent(DatasetStartWrite(obsId, fileId)).as(true)

          override def datasetEndWrite(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
            addEvent(DatasetEndWrite(obsId, fileId)).as(true)

          override def stepContinue(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
            addEvent(StepContinue(obsId)).as(true)

          override def stepPause(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
            addEvent(StepPause(obsId)).as(true)

          override def stepEndObserve(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
            addEvent(StepEndObserve(obsId)).as(true)

          override def stepEndStep(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
            rf.update { s =>
              // This is a hook to let a test caller modify the sequence at the end of a step
              updateStartObserve(s).updateObs(obsId)(_.completeStep(stepId))
            } >> addEvent(StepEndStep(obsId))
              .as(true)

          override def stepAbort(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
            addEvent(StepAbort(obsId)).as(true)

          override def stepStop(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
            addEvent(StepStop(obsId)).as(true)

          override def obsContinue(obsId: Observation.Id): F[Boolean] =
            addEvent(ObsContinue(obsId)).as(true)

          override def obsPause(obsId: Observation.Id): F[Boolean] =
            addEvent(ObsPause(obsId)).as(true)

          override def obsStop(obsId: Observation.Id): F[Boolean] =
            addEvent(ObsStop(obsId)).as(true)

          override def outCapture: F[List[OdbEvent]] = rf.get.map(_.out)

          override def getCurrentRecordedIds: F[ObsRecordedIds] = ObsRecordedIds.Empty.pure[F]
        }
      )

  sealed trait OdbEvent
  case class VisitStart[S](obsId: Observation.Id, staticCfg: S)               extends OdbEvent
  case class SequenceStart(obsId: Observation.Id)                             extends OdbEvent
  case class StepStartStep[D](obsId: Observation.Id)                          extends OdbEvent
  case class StepStartConfigure(obsId: Observation.Id)                        extends OdbEvent
  case class StepEndConfigure(obsId: Observation.Id)                          extends OdbEvent
  case class StepStartObserve(obsId: Observation.Id)                          extends OdbEvent
  case class DatasetStartExposure(obsId: Observation.Id, fileId: ImageFileId) extends OdbEvent
  case class DatasetEndExposure(obsId: Observation.Id, fileId: ImageFileId)   extends OdbEvent
  case class DatasetStartReadout(obsId: Observation.Id, fileId: ImageFileId)  extends OdbEvent
  case class DatasetEndReadout(obsId: Observation.Id, fileId: ImageFileId)    extends OdbEvent
  case class DatasetStartWrite(obsId: Observation.Id, fileId: ImageFileId)    extends OdbEvent
  case class DatasetEndWrite(obsId: Observation.Id, fileId: ImageFileId)      extends OdbEvent
  case class StepContinue(obsId: Observation.Id)                              extends OdbEvent
  case class StepPause(obsId: Observation.Id)                                 extends OdbEvent
  case class StepEndObserve(obsId: Observation.Id)                            extends OdbEvent
  case class StepEndStep(obsId: Observation.Id)                               extends OdbEvent
  case class StepAbort(obsId: Observation.Id)                                 extends OdbEvent
  case class StepStop(obsId: Observation.Id)                                  extends OdbEvent
  case class ObsContinue(obsId: Observation.Id)                               extends OdbEvent
  case class ObsPause(obsId: Observation.Id)                                  extends OdbEvent
  case class ObsStop(obsId: Observation.Id)                                   extends OdbEvent
}
