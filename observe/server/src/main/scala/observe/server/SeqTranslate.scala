// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.Applicative
import cats.data.NonEmptySet
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Sync
import cats.effect.Temporal
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Stream
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.Site
import lucuma.core.enums.StepType as CoreStepType
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
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
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.util.TimeSpan
import lucuma.schemas.model.ModeSignalToNoise
import mouse.all.*
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation as OdbObservation
import observe.model.*
import observe.model.dhs.*
import observe.model.enums.Resource
import observe.model.extensions.*
import observe.server.InstrumentSystem.*
import observe.server.ObserveFailure.Unexpected
import observe.server.altair.Altair
import observe.server.altair.AltairController
import observe.server.altair.AltairControllerDisabled
import observe.server.engine.*
import observe.server.engine.Action.ActionState
import observe.server.flamingos2.Flamingos2
import observe.server.flamingos2.Flamingos2Controller
import observe.server.flamingos2.Flamingos2ControllerDisabled
import observe.server.flamingos2.Flamingos2Header
import observe.server.gcal.*
import observe.server.gems.Gems
import observe.server.gems.GemsController
import observe.server.gems.GemsControllerDisabled
import observe.server.gmos.GmosController.GmosSite
import observe.server.gmos.GmosControllerDisabled
import observe.server.gmos.GmosHeader
import observe.server.gmos.GmosNorth
import observe.server.gmos.GmosNorth.given
import observe.server.gmos.GmosNorthController
import observe.server.gmos.GmosObsKeywordsReader
import observe.server.gmos.GmosSouth
import observe.server.gmos.GmosSouth.given
import observe.server.gmos.GmosSouthController
import observe.server.gmos.NSObserveCommand
import observe.server.gws.GwsHeader
import observe.server.igrins2.Igrins2
import observe.server.igrins2.Igrins2Controller
import observe.server.igrins2.Igrins2ControllerDisabled
import observe.server.igrins2.Igrins2Header
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
    site:                             Site,
    systemss:                         Systems[F],
    gmosNsCmd:                        Ref[F, Option[NSObserveCommand]],
    @annotation.unused conditionsRef: Ref[F, Conditions],
    environment:                      ExecutionEnvironment
  ) extends SeqTranslate[F] {

    private val overriddenSystems = new OverriddenSystems[F](systemss)

    private def translateStep[S, D](
      observation:   OdbObservation,
      atomId:        Atom.Id,
      sequenceType:  SequenceType,
      step:          OdbStep[D],
      signalToNoise: Option[SignalToNoise],
      dataIdx:       PosInt,
      stepType:      StepType,
      insSpec:       InstrumentSpecifics[S, D],
      instf:         SystemOverrides => InstrumentSystem[F],
      instHeader:    KeywordsClient[F] => Header[F],
      mkStepGen:     StepGen.Factory[F, D]
    ): StepGen[F] = {

      def buildStep(
        dataId:    DataId,
        otherSysf: Map[Resource, SystemOverrides => System[F]],
        headers:   SystemOverrides => HeaderExtraData => List[Header[F]],
        stepType:  StepType
      ): StepGen[F] = {

        val configs: Map[Subsystem, SystemOverrides => Action[F]] =
          otherSysf.map { case (r, sf) =>
            val kind = ActionType.Configure(r)
            r -> { (ov: SystemOverrides) =>
              sf(ov).configure.as(Response.Configured(r)).toAction(kind)
            }
          } + (insSpec.instrument -> { (ov: SystemOverrides) =>
            instf(ov).configure
              .as(Response.Configured(insSpec.instrument))
              .toAction(ActionType.Configure(insSpec.instrument))
          })

        def rest(ctx: HeaderExtraData, ov: SystemOverrides): List[ParallelActions[F]] = {
          val inst = instf(ov)
          val env  = ObserveEnvironment(
            systemss.odb,
            overriddenSystems.dhs(ov),
            stepType,
            observation.id,
            instf(ov),
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
          resources = otherSysf.keys.toSet + insSpec.instrument,
          obsControl = (ov: SystemOverrides) => instf(ov).observeControl,
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
        calcSystems(observation, step.telescopeConfig, step.instrumentConfig, stepType, insSpec),
        (ov: SystemOverrides) =>
          calcHeaders(observation, step, stepType, instHeader)(instf(ov).keywordsClient),
        stepType
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
      executionConfig: ExecutionConfig[S, D],
      // Either a Step.Id is specified, or a sequence type to pick the next step from.
      stepIdFrom:      Either[SequenceType, OdbStep.Id],
      insSpec:         InstrumentSpecifics[S, D],
      instf:           (
        SystemOverrides,
        CoreStepType,
        StepType,
        D,
        TelescopeConfig,
        ObserveClass
      ) => InstrumentSystem[
        F
      ],
      instHeader:      D => KeywordsClient[F] => Header[F],
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
          insSpec
            .calcStepType(
              step.stepConfig,
              executionConfig.static,
              step.instrumentConfig,
              step.observeClass
            )
            .map: stepType =>
              translateStep(
                observation,
                atomId,
                seqType,
                step,
                stepSignalToNoise(observation.signalToNoise, step.instrumentConfig, seqType),
                startIdx,
                stepType,
                insSpec,
                (ov: SystemOverrides) =>
                  instf(ov,
                        step.stepConfig.stepType,
                        stepType,
                        step.instrumentConfig,
                        step.telescopeConfig,
                        step.observeClass
                  ),
                instHeader(step.instrumentConfig),
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

    private def extractWavelength[D](dynamicConfig: D): Option[Wavelength] = dynamicConfig match {
      case gn: gmos.DynamicConfig.GmosNorth => gn.centralWavelength
      case gs: gmos.DynamicConfig.GmosSouth => gs.centralWavelength
      case f2: Flamingos2DynamicConfig      => f2.centralWavelength.some
      case i2: Igrins2DynamicConfig         => Igrins2.CentralWavelength
    }

    private def getTcs[S, D](
      subs:            NonEmptySet[TcsController.Subsystem],
      useGaos:         Boolean,
      inst:            InstrumentSpecifics[S, D],
      lsource:         LightSource,
      observation:     OdbObservation,
      telescopeConfig: TelescopeConfig,
      dynamicConfig:   D
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
            LightPath(lsource, inst.sfName(dynamicConfig)),
            extractWavelength(dynamicConfig)
          ): System[F]
        } else { (ov: SystemOverrides) =>
          TcsSouth
            .fromConfig[F](overriddenSystems.tcsSouth(ov), subs, None, inst, systemss.guideDb)(
              observation.targetEnvironment,
              telescopeConfig,
              LightPath(lsource, inst.sfName(dynamicConfig)),
              extractWavelength(dynamicConfig)
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
            LightPath(lsource, inst.sfName(dynamicConfig)),
            extractWavelength(dynamicConfig)
          ): System[F]
        } else { (ov: SystemOverrides) =>
          TcsNorth
            .fromConfig[F](overriddenSystems.tcsNorth(ov), subs, none, inst, systemss.guideDb)(
              observation.targetEnvironment,
              telescopeConfig,
              LightPath(lsource, inst.sfName(dynamicConfig)),
              extractWavelength(dynamicConfig)
            ): System[F]
        }
    }

    private def calcSystems[S, D](
      observation:     OdbObservation,
      telescopeConfig: TelescopeConfig,
      dynamicConfig:   D,
      stepType:        StepType,
      instSpec:        InstrumentSpecifics[S, D]
    ): Map[Resource, SystemOverrides => System[F]] = {

      def adaptGcal(b: GcalController[F] => Gcal[F])(ov: SystemOverrides): Gcal[F] =
        b(overriddenSystems.gcal(ov))

      def defaultGcal: SystemOverrides => Gcal[F] = adaptGcal(Gcal.defaultGcal)

      stepType match {
        case StepType.CelestialObject(inst) =>
          Map(
            Resource.TCS  -> getTcs(
              inst.hasOI.fold(allButGaos, allButGaosNorOi),
              useGaos = false,
              instSpec,
              TcsController.LightSource.Sky,
              observation,
              telescopeConfig,
              dynamicConfig
            ),
            Resource.Gcal -> defaultGcal
          )

        case StepType.NodAndShuffle(inst) =>
          Map(
            Resource.TCS  -> getTcs(
              inst.hasOI.fold(allButGaos, allButGaosNorOi),
              useGaos = false,
              instSpec,
              TcsController.LightSource.Sky,
              observation,
              telescopeConfig,
              dynamicConfig
            ),
            Resource.Gcal -> defaultGcal
          )

        case StepType.FlatOrArc(inst, gcalCfg) =>
          Map(
            Resource.TCS  -> getTcs(
              flatOrArcTcsSubsystems(inst),
              useGaos = false,
              instSpec,
              TcsController.LightSource.GCAL,
              observation,
              telescopeConfig,
              dynamicConfig
            ),
            Resource.Gcal -> adaptGcal(Gcal.fromConfig(site === Site.GS, gcalCfg))
          )

        case StepType.NightFlatOrArc(_, gcalCfg) =>
          Map(
            Resource.TCS  -> getTcs(
              NonEmptySet.of(AGUnit, OIWFS, M2, M1, Mount),
              useGaos = false,
              instSpec,
              TcsController.LightSource.GCAL,
              observation,
              telescopeConfig,
              dynamicConfig
            ),
            Resource.Gcal -> adaptGcal(Gcal.fromConfig(site === Site.GS, gcalCfg))
          )

        case StepType.DarkOrBias(_) => Map.empty[Resource, SystemOverrides => System[F]]

        case StepType.ExclusiveDarkOrBias(_) | StepType.DarkOrBiasNS(_) =>
          Map[Resource, SystemOverrides => System[F]](
            Resource.Gcal -> defaultGcal
          )

        case StepType.AltairObs(inst) =>
          Map(
            Resource.TCS  -> getTcs(
              inst.hasOI.fold(allButGaos, allButGaosNorOi).add(Gaos),
              useGaos = true,
              instSpec,
              TcsController.LightSource.AO,
              observation,
              telescopeConfig,
              dynamicConfig
            ),
            Resource.Gcal -> defaultGcal
          )

        // case StepType.AlignAndCalib => Map.empty[Resource, SystemOverrides => System[F]]

        case StepType.Gems(inst) =>
          Map(
            Resource.TCS  -> getTcs(
              inst.hasOI.fold(allButGaos, allButGaosNorOi).add(Gaos),
              useGaos = true,
              instSpec,
              TcsController.LightSource.AO,
              observation,
              telescopeConfig,
              dynamicConfig
            ),
            Resource.Gcal -> defaultGcal
          )
      }
    }

//    private def calcInstHeader(
//      config:     CleanConfig,
//      instrument: Instrument,
//      kwClient:   KeywordsClient[F]
//    ): Header[F] =
//      instrument match {
//        case Instrument.F2                       =>
//          Flamingos2Header.header[F](kwClient,
//                                     Flamingos2Header.ObsKeywordsReaderODB(config),
//                                     systemss.tcsKeywordReader
//          )
//        case Instrument.GmosS | Instrument.GmosN =>
//          GmosHeader.header[F](kwClient,
//                               GmosObsKeywordsReader(config),
//                               systemss.gmosKeywordReader,
//                               systemss.tcsKeywordReader
//          )
//        case Instrument.Gnirs                    =>
//          GnirsHeader.header[F](kwClient, systemss.gnirsKeywordReader, systemss.tcsKeywordReader)
//        case Instrument.Gpi                      =>
//          GpiHeader.header[F](systemss.gpi.gdsClient,
//                              systemss.tcsKeywordReader,
//                              ObsKeywordReader[F](config, site)
//          )
//        case Instrument.Ghost                    =>
//          GhostHeader.header[F]
//        case Instrument.Niri                     =>
//          NiriHeader.header[F](kwClient, systemss.niriKeywordReader, systemss.tcsKeywordReader)
//        case Instrument.Nifs                     =>
//          NifsHeader.header[F](kwClient, systemss.nifsKeywordReader, systemss.tcsKeywordReader)
//        case Instrument.Gsaoi                    =>
//          GsaoiHeader.header[F](kwClient, systemss.tcsKeywordReader, systemss.gsaoiKeywordReader)
//      }
//
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
      stepType:   StepType,
      instHeader: KeywordsClient[F] => Header[F]
    ): KeywordsClient[F] => HeaderExtraData => List[Header[F]] = stepType match {
      case StepType.CelestialObject(_) | StepType.NodAndShuffle(_) =>
        (kwClient: KeywordsClient[F]) =>
          (ctx: HeaderExtraData) =>
            List(
              commonHeaders(obsCfg, stepCfg, allButGaos.toList, kwClient)(ctx),
              gwsHeaders(kwClient),
              instHeader(kwClient)
            )

      case StepType.AltairObs(_) =>
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

      case StepType.FlatOrArc(inst, _) =>
        (kwClient: KeywordsClient[F]) =>
          (ctx: HeaderExtraData) =>
            List(
              commonHeaders(obsCfg, stepCfg, flatOrArcTcsSubsystems(inst).toList, kwClient)(ctx),
              gcalHeader(kwClient),
              gwsHeaders(kwClient),
              instHeader(kwClient)
            )

      case StepType.NightFlatOrArc(_, _) =>
        (kwClient: KeywordsClient[F]) =>
          (ctx: HeaderExtraData) =>
            List(
              commonHeaders(obsCfg, stepCfg, List(AGUnit, OIWFS, M2, M1, Mount), kwClient)(ctx),
              gcalHeader(kwClient),
              gwsHeaders(kwClient),
              instHeader(kwClient)
            )

      case StepType.DarkOrBias(_) | StepType.DarkOrBiasNS(_) | StepType.ExclusiveDarkOrBias(_) =>
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

      case StepType.Gems(_) =>
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
      odbObsData.executionConfig match {
        case InstrumentExecutionConfig.GmosNorth(executionConfig)  =>
          buildStep[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth](
            odbObsData.observation,
            executionConfig,
            stepIdFrom,
            GmosNorth.specifics,
            (systemOverrides, _, stepType, dynamicConfig, _, _) =>
              GmosNorth.build(
                overriddenSystems.gmosNorth(systemOverrides),
                overriddenSystems.dhs(systemOverrides),
                gmosNsCmd,
                stepType,
                executionConfig.static,
                dynamicConfig
              ),
            (dynamicConfig: gmos.DynamicConfig.GmosNorth) =>
              (kwClient: KeywordsClient[F]) =>
                GmosHeader.header(
                  kwClient,
                  GmosObsKeywordsReader(executionConfig.static, dynamicConfig),
                  systemss.gmosKeywordReader,
                  systemss.tcsKeywordReader
                ),
            StepGen.GmosNorth[F](_, _, _, _, _, _, _, _, _, _, _, _)
          )
        case InstrumentExecutionConfig.GmosSouth(executionConfig)  =>
          buildStep[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth](
            odbObsData.observation,
            executionConfig,
            stepIdFrom,
            GmosSouth.specifics,
            (systemOverrides, _, stepType, dynamicConfig, _, _) =>
              GmosSouth.build(
                overriddenSystems.gmosSouth(systemOverrides),
                overriddenSystems.dhs(systemOverrides),
                gmosNsCmd,
                stepType,
                executionConfig.static,
                dynamicConfig
              ),
            (dynamicConfig: gmos.DynamicConfig.GmosSouth) =>
              (kwClient: KeywordsClient[F]) =>
                GmosHeader.header(
                  kwClient,
                  GmosObsKeywordsReader(executionConfig.static, dynamicConfig),
                  systemss.gmosKeywordReader,
                  systemss.tcsKeywordReader
                ),
            StepGen.GmosSouth[F](_, _, _, _, _, _, _, _, _, _, _, _)
          )
        case InstrumentExecutionConfig.Flamingos2(executionConfig) =>
          buildStep[Flamingos2StaticConfig, Flamingos2DynamicConfig](
            odbObsData.observation,
            executionConfig,
            stepIdFrom,
            Flamingos2.specifics,
            (systemOverrides, coreStepType, _, dynamicConfig, _, _) =>
              Flamingos2.build(
                overriddenSystems.flamingos2(systemOverrides),
                overriddenSystems.dhs(systemOverrides),
                coreStepType,
                dynamicConfig
              ),
            (dynamicConfig: Flamingos2DynamicConfig) =>
              (kwClient: KeywordsClient[F]) =>
                Flamingos2Header.header(
                  kwClient,
                  Flamingos2Header.ObsKeywordsReader(
                    executionConfig.static,
                    dynamicConfig
                  ),
                  systemss.tcsKeywordReader
                ),
            StepGen.Flamingos2[F](_, _, _, _, _, _, _, _, _, _, _, _)
          )
        case InstrumentExecutionConfig.Igrins2(executionConfig)    =>
          buildStep(
            odbObsData.observation,
            executionConfig,
            stepIdFrom,
            Igrins2.specifics,
            (systemOverrides, _, _, dynamicConfig, telescopeConfig, observeClass) =>
              Igrins2.build(
                overriddenSystems.igrins2(systemOverrides),
                dynamicConfig,
                telescopeConfig,
                observeClass
              ),
            (_: Igrins2DynamicConfig) =>
              (kwClient: KeywordsClient[F]) =>
                Igrins2Header.header(
                  kwClient,
                  systemss.tcsKeywordReader
                ),
            StepGen.Igrins2[F](_, _, _, _, _, _, _, _, _, _, _, _)
          )
        case InstrumentExecutionConfig.Ghost(_)                    =>
          ???
      }
  }

  def apply[F[_]: {Async, Logger}](
    site:          Site,
    systems:       Systems[F],
    conditionsRef: Ref[F, Conditions],
    environment:   ExecutionEnvironment
  ): F[SeqTranslate[F]] =
    Ref
      .of[F, Option[NSObserveCommand]](none)
      .map(new SeqTranslateImpl(site, systems, _, conditionsRef, environment))

//  def dataIdFromConfig[F[_]: MonadThrow](config: CleanConfig): F[DataId] =
//    EitherT
//      .fromEither[F](
//        config
//          .extractObsAs[String](DATA_LABEL_PROP)
//          .map(toDataId)
//          .leftMap(e => ObserveFailure.Unexpected(ConfigUtilOps.explain(e)))
//      )
//      .widenRethrowT

  class OverriddenSystems[F[_]: {Sync, Logger}](systems: Systems[F]) {

    private val tcsSouthDisabled: TcsSouthController[F]     = new TcsSouthControllerDisabled[F]
    private val tcsNorthDisabled: TcsNorthController[F]     = new TcsNorthControllerDisabled[F]
    private val gemsDisabled: GemsController[F]             = new GemsControllerDisabled[F]
    private val altairDisabled: AltairController[F]         = new AltairControllerDisabled[F]
    private val dhsDisabled: DhsClientProvider[F]           = (_: String) => new DhsClientDisabled[F]
    private val gcalDisabled: GcalController[F]             = new GcalControllerDisabled[F]
    private val flamingos2Disabled: Flamingos2Controller[F] = new Flamingos2ControllerDisabled[F]
    private val igrins2Disabled: Igrins2Controller[F]       = new Igrins2ControllerDisabled[F]
    private val gmosSouthDisabled: GmosSouthController[F]   =
      new GmosControllerDisabled[F, GmosSite.South.type]("GMOS-S")
    private val gmosNorthDisabled: GmosNorthController[F]   =
      new GmosControllerDisabled[F, GmosSite.North.type]("GMOS-N")
//    private val gsaoiDisabled: GsaoiController[F]           = new GsaoiControllerDisabled[F]
//    private val gpiDisabled: GpiController[F]               = new GpiControllerDisabled[F](systems.gpi.statusDb)
//    private val ghostDisabled: GhostController[F]           = new GhostControllerDisabled[F]
//    private val nifsDisabled: NifsController[F]             = new NifsControllerDisabled[F]
//    private val niriDisabled: NiriController[F]             = new NiriControllerDisabled[F]
//    private val gnirsDisabled: GnirsController[F]           = new GnirsControllerDisabled[F]

    def tcsSouth(overrides: SystemOverrides): TcsSouthController[F] =
      if (overrides.isTcsEnabled.value) systems.tcsSouth
      else tcsSouthDisabled

    def tcsNorth(overrides: SystemOverrides): TcsNorthController[F] =
      if (overrides.isTcsEnabled.value) systems.tcsNorth
      else tcsNorthDisabled

    def gems(overrides: SystemOverrides): GemsController[F] =
      if (overrides.isTcsEnabled.value) systems.gems
      else gemsDisabled

    def altair(overrides: SystemOverrides): AltairController[F] =
      if (overrides.isTcsEnabled.value) systems.altair
      else altairDisabled

    def dhs(overrides: SystemOverrides): DhsClientProvider[F] =
      if (overrides.isDhsEnabled.value) systems.dhs
      else dhsDisabled

    def gcal(overrides: SystemOverrides): GcalController[F] =
      if (overrides.isGcalEnabled.value) systems.gcal
      else gcalDisabled

    def flamingos2(overrides: SystemOverrides): Flamingos2Controller[F] =
      if (overrides.isInstrumentEnabled) systems.flamingos2
      else flamingos2Disabled

    def gmosNorth(overrides: SystemOverrides): GmosNorthController[F] =
      if (overrides.isInstrumentEnabled.value) systems.gmosNorth
      else gmosNorthDisabled

    def gmosSouth(overrides: SystemOverrides): GmosSouthController[F] =
      if (overrides.isInstrumentEnabled.value) systems.gmosSouth
      else gmosSouthDisabled

    def igrins2(overrides: SystemOverrides): Igrins2Controller[F] =
      if (overrides.isInstrumentEnabled.value) systems.igrins2
      else igrins2Disabled

//    def gsaoi(overrides: SystemOverrides): GsaoiController[F] =
//      if (overrides.isInstrumentEnabled) systems.gsaoi
//      else gsaoiDisabled
//
//    def gpi(overrides: SystemOverrides): GpiController[F] =
//      if (overrides.isInstrumentEnabled) systems.gpi
//      else gpiDisabled
//
//    def ghost(overrides: SystemOverrides): GhostController[F] =
//      if (overrides.isInstrumentEnabled) systems.ghost
//      else ghostDisabled
//
//    def nifs(overrides: SystemOverrides): NifsController[F] =
//      if (overrides.isInstrumentEnabled) systems.nifs
//      else nifsDisabled
//
//    def niri(overrides: SystemOverrides): NiriController[F] =
//      if (overrides.isInstrumentEnabled) systems.niri
//      else niriDisabled
//
//    def gnirs(overrides: SystemOverrides): GnirsController[F] =
//      if (overrides.isInstrumentEnabled) systems.gnirs
//      else gnirsDisabled

  }

  def calcStepType(
    inst:       Instrument,
    stepConfig: StepConfig,
    obsClass:   ObserveClass
  ): Either[ObserveFailure, StepType] = stepConfig match {
    case StepConfig.Bias | StepConfig.Dark => StepType.DarkOrBias(inst).asRight
    case c: StepConfig.Gcal                =>
      if (obsClass =!= ObserveClass.DayCal && inst.hasOI) StepType.NightFlatOrArc(inst, c).asRight
      else StepType.FlatOrArc(inst, c).asRight
    case StepConfig.Science                =>
      // TODO: Here goes the logic to differentiate between a non GAOS, GeMS ot Altair observation.
      StepType.CelestialObject(inst).asRight
    case StepConfig.SmartGcal(_)           => Unexpected("Smart GCAL is not supported").asLeft
  }

}
