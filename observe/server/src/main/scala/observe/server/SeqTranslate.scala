// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.Applicative
import cats.data.NonEmptySet
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Temporal
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Stream
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.Site
import lucuma.core.math.SignalToNoise
import lucuma.core.model.sequence
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step as OdbStep
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB.Scalars.Timestamp
import lucuma.schemas.model.ModeSignalToNoise
import mouse.all.*
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation as OdbObservation
import observe.model.*
import observe.model.dhs.*
import observe.model.enums.Resource
import observe.model.extensions.*
import observe.server.InstrumentSystem.*
import observe.server.ObserveFailure.Unexpected
import observe.server.Systems.OverriddenSystems
import observe.server.altair.Altair
import observe.server.engine.*
import observe.server.engine.Action.ActionState
import observe.server.flamingos2.Flamingos2
import observe.server.gcal.*
import observe.server.gems.Gems
import observe.server.ghost.Ghost
import observe.server.gmos.GmosNorth
import observe.server.gmos.GmosSouth
import observe.server.gws.GwsHeader
import observe.server.igrins2.Igrins2
import observe.server.keywords.*
import observe.server.odb.OdbObservationData
import observe.server.tcs.*
import observe.server.tcs.TcsController.LightPath
import observe.server.tcs.TcsController.LightSource
import org.typelevel.log4cats.Logger

trait SeqTranslate[F[_]] {
  def nextStep(
    odbObsData: OdbObservationData,
    stepIdFrom: Either[SequenceType, OdbStep.Id]
  ): (List[Throwable], Option[StepGen[F]])

  def stopObserve(seqId: Observation.Id, graceful: Boolean)(using
    Temporal[F]
  ): EngineState[F] => Stream[F, Event[F]]

  def abortObserve(seqId: Observation.Id)(using
    Temporal[F]
  ): EngineState[F] => Stream[F, Event[F]]

  def pauseObserve(seqId: Observation.Id, graceful: Boolean)(using
    Temporal[F]
  ): EngineState[F] => Stream[F, Event[F]]

  def resumePaused(seqId: Observation.Id)(using
    Temporal[F]
  ): EngineState[F] => Stream[F, Event[F]]

}

object SeqTranslate {

  class SeqTranslateImpl[F[_]: {Async, Logger}](
    site:                   Site,
    systemss:               Systems[F],
    environment:            ExecutionEnvironment,
    instrumentStepBuilders: InstrumentStepBuilders[F]
  ) extends SeqTranslate[F] {

    private val overriddenSystems = new Systems.OverriddenSystems[F](systemss)

    private def translateStep[S, D](
      observation:   OdbObservation,
      atomId:        Atom.Id,
      sequenceType:  SequenceType,
      step:          OdbStep[D],
      signalToNoise: Option[SignalToNoise],
      dataIdx:       PosInt,
      insStep:       InstrumentStep[F],
      mkStepGen:     StepGen.Factory[F, D]
    ): StepGen[F] = {

      def buildStep(
        dataId:    DataId,
        otherSysf: Map[Resource, SystemOverrides => System[F]],
        headers:   SystemOverrides => HeaderExtraData => List[Header[F]]
      ): StepGen[F] = {

        val configs: Map[Subsystem, SystemOverrides => Action[F]] =
          otherSysf.map { case (r, sf) =>
            val kind = ActionType.Configure(r)
            r -> { (ov: SystemOverrides) =>
              sf(ov).configure.as(Response.Configured(r)).toAction(kind)
            }
          } + (insStep.instrument -> { (ov: SystemOverrides) =>
            insStep
              .instrumentSystem(ov)
              .configure
              .as(Response.Configured(insStep.instrument))
              .toAction(ActionType.Configure(insStep.instrument))
          })

        def rest(ctx: HeaderExtraData, ov: SystemOverrides): List[ParallelActions[F]] = {
          val inst = insStep.instrumentSystem(ov)
          val env  = ObserveEnvironment(
            systemss.odb,
            overriddenSystems.dhs(ov),
            insStep.stepType,
            observation.id,
            inst,
            otherSysf.values.toList.map(_(ov)),
            headers(ov),
            ctx,
            environment
          )
          inst.instrumentActions.observeActions(step.id, env)
        }

        mkStepGen(
          atomId = atomId,
          sequenceType = sequenceType,
          id = step.id,
          dataId = dataId,
          resources = otherSysf.keys.toSet + insStep.instrument,
          obsControl = (ov: SystemOverrides) => insStep.instrumentSystem(ov).observeControl,
          generator = StepActionsGen(
            systemss.odb
              .stepStartStep(observation.id, step.id)
              .as(Response.Ignored)
              .toAction(ActionType.OdbEvent),
            systemss.odb
              .stepStartConfigure(observation.id, step.id)
              .as(Response.Ignored)
              .toAction(ActionType.OdbEvent),
            configs,
            systemss.odb
              .stepEndConfigure(observation.id, step.id)
              .as(Response.Ignored)
              .toAction(ActionType.OdbEvent),
            systemss.odb
              .stepStartObserve(observation.id, step.id)
              .as(Response.Ignored)
              .toAction(ActionType.OdbEvent),
            rest,
            systemss.odb
              .stepEndObserve(observation.id, step.id)
              .as(Response.Ignored)
              .toAction(ActionType.OdbEvent)
              .makeUninterruptible,
            systemss.odb
              .stepEndStep(observation.id, step.id)
              .as(Response.Ignored)
              .toAction(ActionType.OdbEvent)
              .makeUninterruptible
          ),
          instConfig = step.instrumentConfig,
          config = step.stepConfig,
          telescopeConfig = step.telescopeConfig,
          signalToNoise = signalToNoise,
          breakpoint = step.breakpoint
        )

      }

      buildStep(
        DataId(s"${observation.title}-$dataIdx"),
        calcSystems(observation, step.telescopeConfig, insStep),
        (ov: SystemOverrides) =>
          calcHeaders(observation, step, insStep.stepType, insStep.instrumentHeader)(
            insStep.instrumentSystem(ov).keywordsClient
          )
      )
    }

    private def findStepInAtom[D](
      atom:   Atom[D],
      stepId: OdbStep.Id
    ): Option[(OdbStep[D], Atom.Id)] =
      atom.steps.find(_.id === stepId).map(s => (s, atom.id))

    private def findStepInSequence[D](
      seq:     ExecutionSequence[D],
      seqType: SequenceType,
      stepId:  OdbStep.Id
    ): Option[(OdbStep[D], Atom.Id, SequenceType)] =
      (seq.nextAtom +: seq.possibleFuture).collectFirstSome: atom =>
        findStepInAtom(atom, stepId).map((s, a) => (s, a, seqType))

    private def findStepInExecutionConfig[S, D](
      execConfig: ExecutionConfig[S, D],
      stepId:     OdbStep.Id
    ): Option[(OdbStep[D], Atom.Id, SequenceType)] =
      execConfig.acquisition
        .flatMap(findStepInSequence(_, SequenceType.Acquisition, stepId))
        .orElse(execConfig.science.flatMap(findStepInSequence(_, SequenceType.Science, stepId)))

    private def nextStepFromExecutionSequence[D](
      execSequence: Option[ExecutionSequence[D]],
      seqType:      SequenceType
    ): Option[(OdbStep[D], Atom.Id, SequenceType)] =
      execSequence.map(seq => (seq.nextAtom.steps.head, seq.nextAtom.id, seqType))

    private def nextStepFromSequenceType[S, D](
      execConfig: ExecutionConfig[S, D],
      seqType:    SequenceType
    ): Option[(OdbStep[D], Atom.Id, SequenceType)] =
      seqType match
        case SequenceType.Acquisition =>
          nextStepFromExecutionSequence(execConfig.acquisition, SequenceType.Acquisition)
            .orElse(nextStepFromExecutionSequence(execConfig.science, SequenceType.Science))
        case SequenceType.Science     =>
          nextStepFromExecutionSequence(execConfig.science, SequenceType.Science)

    def stepSignalToNoise[D](
      modeSignalToNoise: ModeSignalToNoise,
      instrumentConfig:  D,
      sequenceType:      SequenceType
    ): Option[SignalToNoise] =
      (modeSignalToNoise, instrumentConfig) match
        case (ModeSignalToNoise.Spectroscopy(acquisition, science),
              _: gmos.DynamicConfig | Flamingos2DynamicConfig
            ) =>
          sequenceType match
            case SequenceType.Acquisition => acquisition.map(_.single.value)
            case SequenceType.Science     => science.map(_.single.value)
        case (ModeSignalToNoise.GmosNorthImaging(snByFilter),
              gnConfig: gmos.DynamicConfig.GmosNorth
            ) =>
          gnConfig.filter.flatMap(snByFilter.get(_)).map(_.single.value)
        case (ModeSignalToNoise.GmosSouthImaging(snByFilter),
              gsConfig: gmos.DynamicConfig.GmosSouth
            ) =>
          gsConfig.filter.flatMap(snByFilter.get(_)).map(_.single.value)
        // Step/SN mismatch or unsupported instrument
        case _ => none

    private def buildStep[S, D](
      observation:     OdbObservation,
      obsTime:         Timestamp,
      executionConfig: ExecutionConfig[S, D],
      // Either a Step.Id is specified, or a sequence type to pick the next step from.
      stepIdFrom:      Either[SequenceType, OdbStep.Id],
      insStepBuilder:  InstrumentStepBuilder[F, S, D],
      mkStepGen:       StepGen.Factory[F, D],
      startIdx:        PosInt = PosInt.unsafeFrom(1)
    ): (List[Throwable], Option[StepGen[F]]) = {
      val stepInfo: Option[(OdbStep[D], Atom.Id, SequenceType)] =
        stepIdFrom.fold(
          nextStepFromSequenceType(executionConfig, _),
          findStepInExecutionConfig(executionConfig, _)
        )

      stepInfo
        .map: (step, atomId, seqType) =>
          insStepBuilder
            .build(OverriddenSystems[F](systemss),
                   step.stepConfig.stepType,
                   observation.targetEnvironment,
                   executionConfig.static,
                   step,
                   obsTime
            )
            .map: insStep =>
              translateStep(
                observation,
                atomId,
                seqType,
                step,
                stepSignalToNoise(observation.signalToNoise, step.instrumentConfig, seqType),
                startIdx,
                insStep,
                mkStepGen
              )
            .fold(
              err => (List(err), none),
              cs => (List.empty, cs.some)
            )
        .getOrElse((List.empty, none))
    }

    private def deliverObserveCmd(seqId: Observation.Id, f: ObserveControl[F] => F[Unit])(
      st: EngineState[F]
    ): Option[Stream[F, Event[F]]] = {

      def isObserving(v: Action[F]): Boolean =
        v.kind === ActionType.Observe && v.state.runState.started

      for {
        obsSeq <- st.sequences.get(seqId)
        if obsSeq.seq.currentExecution.execution
          .exists(isObserving)
        curStp <- obsSeq.loadedStep
        obsCtr  = curStp.obsControl
      } yield Stream.eval(
        f(obsCtr(obsSeq.overrides)).attempt
          .flatMap(handleError)
      )
    }

    private def handleError: Either[Throwable, Unit] => F[Event[F]] = {
      case Left(e: ObserveFailure) => Event.logErrorMsgF(ObserveFailure.explain(e))
      case Left(e: Throwable)      =>
        Event.logErrorMsgF(ObserveFailure.explain(ObserveFailure.ObserveException(e)))
      case _                       => Event.nullEvent[F].pure[F].widen[Event[F]]
    }

    override def stopObserve(obsId: Observation.Id, graceful: Boolean)(using
      tio: Temporal[F]
    ): EngineState[F] => Stream[F, Event[F]] = st => {
      def f(oc: ObserveControl[F]): F[Unit] = oc match {
        case CompleteControl(StopObserveCmd(stop), _, _, _, _, _) => stop(graceful)
        case UnpausableControl(StopObserveCmd(stop), _)           => stop(graceful)
        case _                                                    => Applicative[F].unit
      }
      deliverObserveCmd(obsId, f)(st).getOrElse(stopPaused(obsId).apply(st))
    }

    override def abortObserve(obsId: Observation.Id)(using
      tio: Temporal[F]
    ): EngineState[F] => Stream[F, Event[F]] = st => {
      def f(oc: ObserveControl[F]): F[Unit] = oc match {
        case CompleteControl(_, AbortObserveCmd(abort), _, _, _, _) => abort
        case UnpausableControl(_, AbortObserveCmd(abort))           => abort
        case _                                                      => Applicative[F].unit
      }

      deliverObserveCmd(obsId, f)(st).getOrElse(abortPaused(obsId).apply(st))
    }

    override def pauseObserve(obsId: Observation.Id, graceful: Boolean)(using
      tio: Temporal[F]
    ): EngineState[F] => Stream[F, Event[F]] = {
      def f(oc: ObserveControl[F]): F[Unit] = oc match {
        case CompleteControl(_, _, PauseObserveCmd(pause), _, _, _) => pause(graceful)
        case _                                                      => Applicative[F].unit
      }
      deliverObserveCmd(obsId, f).map(_.orEmpty)
    }

    override def resumePaused(obsId: Observation.Id)(using
      tio: Temporal[F]
    ): EngineState[F] => Stream[F, Event[F]] = (st: EngineState[F]) => {
      val observeIndex: Option[(ObserveContext[F], Option[TimeSpan], Int)] =
        st.sequences
          .get(obsId)
          .flatMap(
            _.seq.currentExecution.execution.zipWithIndex
              .find(_._1.kind === ActionType.Observe)
              .flatMap { case (a, i) =>
                a.state.runState match {
                  case ActionState.Paused(c: ObserveContext[F] @unchecked) =>
                    (c,
                     a.state.partials.collectFirst { case x: Progress =>
                       x.progress
                     },
                     i
                    ).some
                  case _                                                   => none
                }
              }
          )

      observeIndex.map { case (obCtx, t, i) =>
        Stream.emit[F, Event[F]](
          Event.actionResume[F](
            obsId,
            i,
            obCtx
              .progress(ElapsedTime(t.getOrElse(TimeSpan.Zero)))
              .mergeHaltR(obCtx.resumePaused(obCtx.expTime))
              .handleErrorWith(catchObsErrors[F])
          )
        )
      }.orEmpty
    }

    private def endPaused(obsId: Observation.Id, l: ObserveContext[F] => Stream[F, Result])(
      st: EngineState[F]
    ): Stream[F, Event[F]] =
      st.sequences
        .get(obsId)
        .flatMap(
          _.seq.currentExecution.execution.zipWithIndex
            .find(_._1.kind === ActionType.Observe)
            .flatMap { case (a, i) =>
              a.state.runState match {
                case ActionState.Paused(c: ObserveContext[F] @unchecked) =>
                  Stream
                    .eval(
                      Event
                        .actionResume(
                          obsId,
                          i,
                          c.progress(ElapsedTime(c.expTime))
                            .mergeHaltR(l(c))
                            .handleErrorWith(catchObsErrors[F])
                        )
                        .pure[F]
                    )
                    .some
                case _                                                   => none
              }
            }
        )
        .orEmpty

    private def stopPaused(
      obsId: Observation.Id
    ): EngineState[F] => Stream[F, Event[F]] =
      endPaused(obsId, _.stopPaused)

    private def abortPaused(
      obsId: Observation.Id
    ): EngineState[F] => Stream[F, Event[F]] =
      endPaused(obsId, _.abortPaused)

    import TcsController.Subsystem.*

    private def flatOrArcTcsSubsystems(inst: Instrument): NonEmptySet[TcsController.Subsystem] =
      NonEmptySet.of(AGUnit, (if (inst.hasOI) List(OIWFS) else List.empty)*)

    private def getTcs(
      subs:            NonEmptySet[TcsController.Subsystem],
      useGaos:         Boolean,
      inst:            InstrumentStep[F],
      lsource:         LightSource,
      observation:     OdbObservation,
      telescopeConfig: TelescopeConfig
    ): SystemOverrides => System[F] = site match {
      case Site.GS =>
        if (useGaos) { (ov: SystemOverrides) =>
          TcsSouth.fromConfig[F](
            overriddenSystems.tcsSouth(ov),
            subs,
            Gems.fromConfig[F](overriddenSystems.gems(ov), systemss.guideDb).some,
            inst,
            systemss.guideDb
          )(
            observation.targetEnvironment,
            telescopeConfig,
            LightPath(lsource, inst.sfName),
            inst.centralWavelength,
            inst.defocusB
          ): System[F]
        } else { (ov: SystemOverrides) =>
          TcsSouth
            .fromConfig[F](overriddenSystems.tcsSouth(ov), subs, None, inst, systemss.guideDb)(
              observation.targetEnvironment,
              telescopeConfig,
              LightPath(lsource, inst.sfName),
              inst.centralWavelength,
              inst.defocusB
            ): System[F]
        }

      case Site.GN =>
        if (useGaos) { (ov: SystemOverrides) =>
          TcsNorth.fromConfig[F](
            overriddenSystems.tcsNorth(ov),
            subs,
            Altair(overriddenSystems.altair(ov)).some,
            inst,
            systemss.guideDb
          )(
            observation.targetEnvironment,
            telescopeConfig,
            LightPath(lsource, inst.sfName),
            inst.centralWavelength,
            inst.defocusB
          ): System[F]
        } else { (ov: SystemOverrides) =>
          TcsNorth
            .fromConfig[F](overriddenSystems.tcsNorth(ov), subs, none, inst, systemss.guideDb)(
              observation.targetEnvironment,
              telescopeConfig,
              LightPath(lsource, inst.sfName),
              inst.centralWavelength,
              inst.defocusB
            ): System[F]
        }
    }

    private def calcSystems[S](
      observation:     OdbObservation,
      telescopeConfig: TelescopeConfig,
      insStep:         InstrumentStep[F]
    ): Map[Resource, SystemOverrides => System[F]] = {

      def adaptGcal(b: GcalController[F] => Gcal[F])(ov: SystemOverrides): Gcal[F] =
        b(overriddenSystems.gcal(ov))

      def defaultGcal: SystemOverrides => Gcal[F] = adaptGcal(Gcal.defaultGcal)

      insStep.stepType match {
        case StepKind.CelestialObject(inst) =>
          Map(
            Resource.TCS  -> getTcs(
              inst.hasOI.fold(allButGaos, allButGaosNorOi),
              useGaos = false,
              insStep,
              TcsController.LightSource.Sky,
              observation,
              telescopeConfig
            ),
            Resource.Gcal -> defaultGcal
          )

        case StepKind.NodAndShuffle(inst) =>
          Map(
            Resource.TCS  -> getTcs(
              inst.hasOI.fold(allButGaos, allButGaosNorOi),
              useGaos = false,
              insStep,
              TcsController.LightSource.Sky,
              observation,
              telescopeConfig
            ),
            Resource.Gcal -> defaultGcal
          )

        case StepKind.FlatOrArc(inst, gcalCfg) =>
          Map(
            Resource.TCS  -> getTcs(
              flatOrArcTcsSubsystems(inst),
              useGaos = false,
              insStep,
              TcsController.LightSource.GCAL,
              observation,
              telescopeConfig
            ),
            Resource.Gcal -> adaptGcal(Gcal.fromConfig(site === Site.GS, gcalCfg))
          )

        case StepKind.NightFlatOrArc(_, gcalCfg) =>
          Map(
            Resource.TCS  -> getTcs(
              NonEmptySet.of(AGUnit, OIWFS, M2, M1, Mount),
              useGaos = false,
              insStep,
              TcsController.LightSource.GCAL,
              observation,
              telescopeConfig
            ),
            Resource.Gcal -> adaptGcal(Gcal.fromConfig(site === Site.GS, gcalCfg))
          )

        case StepKind.DarkOrBias(_) => Map.empty[Resource, SystemOverrides => System[F]]

        case StepKind.ExclusiveDarkOrBias(_) | StepKind.DarkOrBiasNS(_) =>
          Map[Resource, SystemOverrides => System[F]](
            Resource.Gcal -> defaultGcal
          )

        case StepKind.AltairObs(inst) =>
          Map(
            Resource.TCS  -> getTcs(
              inst.hasOI.fold(allButGaos, allButGaosNorOi).add(Gaos),
              useGaos = true,
              insStep,
              TcsController.LightSource.AO,
              observation,
              telescopeConfig
            ),
            Resource.Gcal -> defaultGcal
          )

        // case StepType.AlignAndCalib => Map.empty[Resource, SystemOverrides => System[F]]

        case StepKind.Gems(inst) =>
          Map(
            Resource.TCS  -> getTcs(
              inst.hasOI.fold(allButGaos, allButGaosNorOi).add(Gaos),
              useGaos = true,
              insStep,
              TcsController.LightSource.AO,
              observation,
              telescopeConfig
            ),
            Resource.Gcal -> defaultGcal
          )
      }
    }

    private def commonHeaders[D](
      obsCfg:        OdbObservation,
      stepCfg:       OdbStep[D],
      tcsSubsystems: List[TcsController.Subsystem],
      kwClient:      KeywordsClient[F]
    )(ctx: HeaderExtraData): Header[F] =
      new StandardHeader(
        kwClient,
        ObsKeywordReader[F, D](obsCfg, stepCfg, site, systemss),
        systemss.tcsKeywordReader,
        StateKeywordsReader[F](
          systemss.conditionSetReader(ctx.conditions),
          ctx.operator,
          ctx.observer,
          site
        ),
        tcsSubsystems
      )

    private def gwsHeaders(kwClient: KeywordsClient[F]): Header[F] =
      GwsHeader.header(kwClient, systemss.gwsKeywordReader)

    private def gcalHeader(kwClient: KeywordsClient[F]): Header[F] =
      GcalHeader.header(kwClient, systemss.gcalKeywordReader)

//    private def altairHeader(kwClient: KeywordsClient[F]): Header[F] =
//      AltairHeader.header[F](
//        kwClient,
//        systemss.altairKeywordReader,
//        systemss.tcsKeywordReader
//      )
//
//    private def altairLgsHeader(guideStar: GuideStarType, kwClient: KeywordsClient[F]): Header[F] =
//      if (guideStar === GuideStarType.LGS) {
//        AltairLgsHeader.header(kwClient, systemss.altairKeywordReader)
//      } else {
//        dummyHeader[F]
//      }
//
    private def calcHeaders[D](
      obsCfg:     OdbObservation,
      stepCfg:    OdbStep[D],
      stepType:   StepKind,
      instHeader: KeywordsClient[F] => Header[F]
    ): KeywordsClient[F] => HeaderExtraData => List[Header[F]] = stepType match {
      case StepKind.CelestialObject(_) | StepKind.NodAndShuffle(_) =>
        (kwClient: KeywordsClient[F]) =>
          (ctx: HeaderExtraData) =>
            List(
              commonHeaders(obsCfg, stepCfg, allButGaos.toList, kwClient)(ctx),
              gwsHeaders(kwClient),
              instHeader(kwClient)
            )

      case StepKind.AltairObs(_) =>
        (kwClient: KeywordsClient[F]) =>
          (ctx: HeaderExtraData) =>
            // Order is important
            List(
              commonHeaders(obsCfg, stepCfg, allButGaos.toList, kwClient)(ctx),
//              altairHeader(kwClient),
//              altairLgsHeader(Altair.guideStarType(obsCfg), kwClient),
              gwsHeaders(kwClient),
              instHeader(kwClient)
            )

      case StepKind.FlatOrArc(inst, _) =>
        (kwClient: KeywordsClient[F]) =>
          (ctx: HeaderExtraData) =>
            List(
              commonHeaders(obsCfg, stepCfg, flatOrArcTcsSubsystems(inst).toList, kwClient)(ctx),
              gcalHeader(kwClient),
              gwsHeaders(kwClient),
              instHeader(kwClient)
            )

      case StepKind.NightFlatOrArc(_, _) =>
        (kwClient: KeywordsClient[F]) =>
          (ctx: HeaderExtraData) =>
            List(
              commonHeaders(obsCfg, stepCfg, List(AGUnit, OIWFS, M2, M1, Mount), kwClient)(ctx),
              gcalHeader(kwClient),
              gwsHeaders(kwClient),
              instHeader(kwClient)
            )

      case StepKind.DarkOrBias(_) | StepKind.DarkOrBiasNS(_) | StepKind.ExclusiveDarkOrBias(_) =>
        (kwClient: KeywordsClient[F]) =>
          (ctx: HeaderExtraData) =>
            List(
              commonHeaders(obsCfg, stepCfg, Nil, kwClient)(ctx),
              gwsHeaders(kwClient),
              instHeader(kwClient)
            )

//      case StepType.AlignAndCalib =>
//        (_: KeywordsClient[F]) =>
//          (_: HeaderExtraData) => List.empty[Header[F]] // No headers for A&C

      case StepKind.Gems(_) =>
        (kwClient: KeywordsClient[F]) =>
          (ctx: HeaderExtraData) =>
            List(
              commonHeaders(obsCfg, stepCfg, allButGaos.toList, kwClient)(ctx),
              gwsHeaders(kwClient),
//            gemsHeaders(kwClient, ObsKeywordReader[F](config, site), systemss.tcsKeywordReader),
              instHeader(kwClient)
            )
    }

    override def nextStep(
      odbObsData: OdbObservationData,
      stepIdFrom: Either[SequenceType, OdbStep.Id]
    ): (List[Throwable], Option[StepGen[F]]) =
      odbObsData.observation.observationTime
        .map { obsTime =>
          odbObsData.executionConfig match {
            case InstrumentExecutionConfig.GmosNorth(executionConfig)  =>
              buildStep[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth](
                odbObsData.observation,
                obsTime,
                executionConfig,
                stepIdFrom,
                instrumentStepBuilders.gmosNorth,
                StepGen.GmosNorth[F]
              )
            case InstrumentExecutionConfig.GmosSouth(executionConfig)  =>
              buildStep[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth](
                odbObsData.observation,
                obsTime,
                executionConfig,
                stepIdFrom,
                instrumentStepBuilders.gmosSouth,
                StepGen.GmosSouth[F](_, _, _, _, _, _, _, _, _, _, _, _)
              )
            case InstrumentExecutionConfig.Flamingos2(executionConfig) =>
              buildStep[Flamingos2StaticConfig, Flamingos2DynamicConfig](
                odbObsData.observation,
                obsTime,
                executionConfig,
                stepIdFrom,
                instrumentStepBuilders.flamingos2,
                StepGen.Flamingos2[F](_, _, _, _, _, _, _, _, _, _, _, _)
              )
            case InstrumentExecutionConfig.Igrins2(executionConfig)    =>
              buildStep(
                odbObsData.observation,
                obsTime,
                executionConfig,
                stepIdFrom,
                instrumentStepBuilders.igrins2,
                StepGen.Igrins2[F](_, _, _, _, _, _, _, _, _, _, _, _)
              )
            case InstrumentExecutionConfig.Ghost(executionConfig)      =>
              buildStep(
                odbObsData.observation,
                obsTime,
                executionConfig,
                stepIdFrom,
                instrumentStepBuilders.ghost,
                StepGen.Ghost[F](_, _, _, _, _, _, _, _, _, _, _, _)
              )
            case InstrumentExecutionConfig.Gnirs(_)                    => ???
            case InstrumentExecutionConfig.Visitor(_)                  => ???
          }
        }
        .getOrElse(
          (List(
             ObserveFailure
               .OdbSeqError(s"Observing time not set for observation ${odbObsData.observation.id}")
           ),
           none
          )
        )
  }

  def apply[F[_]: {Async, Logger}](
    site:          Site,
    systems:       Systems[F],
    conditionsRef: Ref[F, CurrentConditions],
    environment:   ExecutionEnvironment
  ): F[SeqTranslate[F]] = (
    for {
      gmosN   <- GmosNorth.build
      gmosS   <- GmosSouth.build
      f2      <- Flamingos2.build
      ghost   <- Ghost.instrumentStepBuilder(conditionsRef)
      igrins2 <- Igrins2.build
    } yield InstrumentStepBuilders[F](
      f2,
      gmosN,
      gmosS,
      ghost,
      igrins2
    )
  ).map(x => new SeqTranslateImpl(site, systems, environment, x))

  def calcStepType(
    inst:       Instrument,
    stepConfig: StepConfig,
    obsClass:   ObserveClass
  ): Either[ObserveFailure, StepKind] = stepConfig match {
    case StepConfig.Bias | StepConfig.Dark => StepKind.DarkOrBias(inst).asRight
    case c: StepConfig.Gcal                =>
      if (obsClass =!= ObserveClass.DayCal && inst.hasOI) StepKind.NightFlatOrArc(inst, c).asRight
      else StepKind.FlatOrArc(inst, c).asRight
    case StepConfig.Science                =>
      // TODO: Here goes the logic to differentiate between a non GAOS, GeMS ot Altair observation.
      StepKind.CelestialObject(inst).asRight
    case StepConfig.SmartGcal(_)           => Unexpected("Smart GCAL is not supported").asLeft
  }

  case class InstrumentStepBuilders[F[_]](
    flamingos2: InstrumentStepBuilder[F, Flamingos2StaticConfig, Flamingos2DynamicConfig],
    gmosNorth:  InstrumentStepBuilder[F, gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth],
    gmosSouth:  InstrumentStepBuilder[F, gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth],
    ghost:      InstrumentStepBuilder[F, GhostStaticConfig, GhostDynamicConfig],
    igrins2:    InstrumentStepBuilder[F, Igrins2StaticConfig, Igrins2DynamicConfig]
  )

}
