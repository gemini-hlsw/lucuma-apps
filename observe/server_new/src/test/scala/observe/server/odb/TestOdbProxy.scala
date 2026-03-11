// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.effect.Ref
import cats.effect.kernel.Resource
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosLong
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
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.refined.auto.*
import lucuma.schemas.model.ModeSignalToNoise
import monocle.Focus
import monocle.Lens
import monocle.syntax.all.focus
import observe.common.EventsGQL.RecordDatasetMutation.Data.RecordDataset.Dataset
import observe.common.ObsQueriesGQL.ObsQuery.Data
import observe.common.ObsQueriesGQL.ObsQuery.Data.Observation as ODBObservation
import observe.common.ObsQueriesGQL.ObsQuery.Data.Observation.TargetEnvironment.GuideEnvironment
import observe.model.dhs.ImageFileId
import observe.model.odb.ObsRecordedIds

trait TestOdbProxy[F[_]] extends OdbProxy[F]:
  def outCapture: F[List[TestOdbProxy.OdbEvent]]

object TestOdbProxy {

  case class State(
    acquisition: Option[Atom[DynamicConfig.GmosNorth]],
    sciences:    List[Atom[DynamicConfig.GmosNorth]],
    out:         List[OdbEvent]
  ) {
    private def completeStepInAtom(
      stepId: Step.Id
    )(a: Atom[DynamicConfig.GmosNorth]): Option[Atom[DynamicConfig.GmosNorth]] =
      NonEmptyList
        .fromList(a.steps.filterNot(_.id === stepId))
        .map: newSteps =>
          a.copy(steps = newSteps)

    def completeStep(stepId: Step.Id): State =
      copy(
        acquisition = acquisition.flatMap(completeStepInAtom(stepId)),
        sciences = sciences.map(completeStepInAtom(stepId)).flattenOption
      )

    def resetAcquisition(original: Option[Atom[DynamicConfig.GmosNorth]]): State =
      copy(acquisition = original)
  }

  object State:
    val sciences: Lens[State, List[Atom[DynamicConfig.GmosNorth]]] = Focus[State](_.sciences)

  def build[F[_]: Concurrent](
    staticCfg:          StaticConfig.GmosNorth,
    acquisition:        Option[Atom[DynamicConfig.GmosNorth]],
    sciences:           List[Atom[DynamicConfig.GmosNorth]] = List.empty,
    updateStartObserve: State => State = identity
  ): F[TestOdbProxy[F]] = Ref
    .of[F, State](State(acquisition, sciences, List.empty))
    .map(rf =>
      new TestOdbProxy[F] {
        override def obsEditSubscription(obsId: Observation.Id): Resource[F, fs2.Stream[F, Unit]] =
          Resource.pure(fs2.Stream.empty)

        private def addEvent(ev: OdbEvent): F[Unit] =
          rf.modify(s => (s.focus(_.out).modify(_.appended(ev)), ()))

        override def resetAcquisition(oid: Observation.Id): F[Unit] =
          rf.update(_.resetAcquisition(acquisition))

        override def read(oid: Observation.Id): F[OdbObservationData] =
          rf.get
            .map { st =>
              val sciAtom: Option[Atom[DynamicConfig.GmosNorth]] = st.sciences.headOption
              val sciTail: List[Atom[DynamicConfig.GmosNorth]]   =
                st.sciences match
                  case head :: tail => tail
                  case Nil          => Nil

              OdbObservationData(
                Data.Observation(
                  oid,
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
                InstrumentExecutionConfig.GmosNorth(
                  ExecutionConfig[StaticConfig.GmosNorth, DynamicConfig.GmosNorth](
                    staticCfg,
                    st.acquisition.map(
                      ExecutionSequence[DynamicConfig.GmosNorth](_, List.empty, true)
                    ),
                    sciAtom.map(
                      ExecutionSequence[DynamicConfig.GmosNorth](
                        _,
                        sciTail,
                        sciTail.nonEmpty
                      )
                    )
                  )
                )
              )
            }

        override def visitStart[S](obsId: Observation.Id, staticCfg: S): F[Unit] = addEvent(
          VisitStart(obsId, staticCfg)
        )

        override def sequenceStart(obsId: Observation.Id): F[Unit] = addEvent(SequenceStart(obsId))

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
          rf.update { a =>
            // This is a hook to let a test caller modify the sequence at the end of a step
            updateStartObserve(a).completeStep(stepId)
          } *> addEvent(StepEndStep(obsId))
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
