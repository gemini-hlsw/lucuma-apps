// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s

import cats.MonadThrow
import cats.effect.Sync
import cats.syntax.all.*
import fs2.Stream
import grackle.Query.Binding
import grackle.QueryCompiler.Elab
import grackle.QueryCompiler.SelectElaborator
import grackle.Result
import grackle.Schema
import grackle.TypeRef
import grackle.circe.CirceMapping
import grackle.syntax.given
import io.circe.Encoder
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.OffsetInput
import lucuma.odb.graphql.input.TimeSpanInput
import lucuma.odb.graphql.input.WavelengthInput
import lucuma.odb.graphql.schema.SchemaStitcher
import mouse.boolean.given
import navigate.model.CommandResult
import navigate.model.ServerConfiguration
import navigate.model.config.NavigateConfiguration
import navigate.model.enums.AcquisitionAdjustmentCommand
import navigate.server.NavigateEngine
import navigate.web.server.OcsBuildInfo
import navigate.web.server.http4s.input.*
import org.tpolecat.typename.TypeName
import org.typelevel.log4cats.Logger

import scala.reflect.ClassTag

import encoder.given

class NavigateMappings[F[_]: Sync](
  config:              NavigateConfiguration,
  val server:          NavigateEngine[F],
  val topics:          TopicManager[F]
)(
  override val schema: Schema
) extends CirceMapping[F] {
  import NavigateMappings._

  val QueryType: TypeRef        = schema.ref("Query")
  val MutationType: TypeRef     = schema.ref("Mutation")
  val SubscriptionType: TypeRef = schema.ref("Subscription")

  /** A root field and the elaborator for its arguments. */
  private case class RootField(
    elaborator:   PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]],
    fieldMapping: RootEffect
  )

  private type Args[A] = PartialFunction[List[Binding], Result[A]]

  /** Parses the only argument of a root field, `name`, with `matcher`. */
  private def arg[A](name: String, matcher: Matcher[A]): Args[A] = {
    case List(matcher(`name`, r)) => r
  }

  /** Parses the two arguments of a root field, in schema order. */
  private def args[A, B](na: String, ma: Matcher[A], nb: String, mb: Matcher[B]): Args[(A, B)] = {
    case List(ma(`na`, ra), mb(`nb`, rb)) => (ra, rb).parTupled
  }

  /** Parses the three arguments of a root field, in schema order. */
  private def args[A, B, C](
    na: String,
    ma: Matcher[A],
    nb: String,
    mb: Matcher[B],
    nc: String,
    mc: Matcher[C]
  ): Args[(A, B, C)] = { case List(ma(`na`, ra), mb(`nb`, rb), mc(`nc`, rc)) =>
    (ra, rb, rc).parTupled
  }

  /**
   * A root field with arguments. `args` parses them, and `run` gets the result.
   *
   * Unexpected arguments are internal errors.
   */
  private def rootField[I: {ClassTag, TypeName}, A: Encoder](
    tpe:   TypeRef,
    field: String,
    args:  Args[I]
  )(run: I => F[Result[A]]): RootField =
    RootField(
      { case (`tpe`, `field`, bindings) =>
        Elab
          .liftR(
            args.applyOrElse(
              bindings,
              bs => Result.internalError(s"Unexpected arguments for $field: $bs")
            )
          )
          .flatMap(i => Elab.env(ArgsKey -> i))
      },
      RootEffect.computeEncodable(field)((_, env) => env.getR[I](ArgsKey).flatTraverse(run))
    )

  /** A query without arguments. Errors from `run` become GraphQL errors. */
  private def query[A: Encoder](field: String)(run: => F[A]): RootField =
    RootField(
      PartialFunction.empty,
      RootEffect.computeEncodable(field)((_, _) => run.attemptResult)
    )

  /**
   * A mutation without arguments.
   *
   * Failed commands are GraphQL errors. Exceptions in `run` are internal errors.
   */
  private def command(field: String)(run: => F[CommandResult]): RootField =
    RootField(
      PartialFunction.empty,
      RootEffect.computeEncodable(field)((_, _) => run.attemptResultOutcome)
    )

  /** A mutation with arguments. Same as `command`, but `run` gets the parsed arguments. */
  private def command[I: {ClassTag, TypeName}](field: String, args: Args[I])(
    run: I => F[CommandResult]
  ): RootField =
    rootField(MutationType, field, args)(run(_).attemptResultOutcome)

  private val queryFields: List[RootField] = List(
    query("telescopeState")(server.getTelescopeState),
    query("guideState")(server.getGuideState),
    query("guidersQualityValues")(server.getGuidersQuality),
    query("navigateState")(server.getNavigateState),
    rootField(QueryType, "instrumentPort", arg("instrument", InstrumentBinding))(
      server.getInstrumentPort(_).attemptResult
    ),
    query("serverVersion")(OcsBuildInfo.version.pure[F]),
    query("targetAdjustmentOffsets")(server.getTargetAdjustments),
    query("originAdjustmentOffset")(server.getOriginOffset),
    query("pointingAdjustmentOffset")(server.getPointingOffset),
    query("serverConfiguration")(
      ServerConfiguration(
        OcsBuildInfo.version,
        config.site,
        config.navigateEngine.odb.toString,
        config.lucumaSSO.ssoUrl.toString
      ).pure[F]
    ),
    query("acMechsState")(server.getAcMechsState),
    query("pwfs1MechsState")(server.getPwfs1MechsState),
    query("pwfs2MechsState")(server.getPwfs2MechsState),
    query("bafflesState")(server.getBafflesState),
    query("pwfs1ConfigState")(server.getPwfs1Configuration),
    query("pwfs2ConfigState")(server.getPwfs2Configuration),
    query("oiwfsConfigState")(server.getOiwfsConfiguration)
  )

  private val mutationFields: List[RootField] = List(
    command("mountPark")(server.mcsPark),
    command("mountFollow", arg("enable", BooleanBinding))(server.mcsFollow),
    command("mountUnwrap")(server.mcsUnwrap),
    command("rotatorPark")(server.rotPark),
    command("rotatorFollow", arg("enable", BooleanBinding))(server.rotFollow),
    command("rotatorConfig", arg("config", RotatorTrackingInput.Binding))(server.rotTrackingConfig),
    command("rotatorUnwrap")(server.rotUnwrap),
    command("scsFollow", arg("enable", BooleanBinding))(server.scsFollow),
    command("tcsConfig", arg("config", TcsConfigInput.Binding))(server.tcsConfig),
    command(
      "slew",
      args(
        "slewOptions",
        SlewOptionsInput.Binding,
        "config",
        TcsConfigInput.Binding,
        "obsId",
        ObservationIdBinding.Option
      )
    )((slewOptions, config, obsId) => server.slew(slewOptions, config, obsId)),
    command("swapTarget", arg("swapConfig", SwapConfigInput.Binding))(server.swapTarget),
    command("restoreTarget", arg("config", TcsConfigInput.Binding))(server.restoreTarget),
    command(
      "instrumentSpecifics",
      arg("instrumentSpecificsParams", InstrumentSpecificsInput.Binding)
    )(server.instrumentSpecifics),
    // PWFS1
    command("pwfs1Target", arg("target", TargetPropertiesInput.Binding))(server.pwfs1Target),
    command("pwfs1ProbeTracking", arg("config", ProbeTrackingInput.Binding))(
      server.pwfs1ProbeTracking
    ),
    command("pwfs1Park")(server.pwfs1Park),
    command("pwfs1Follow", arg("enable", BooleanBinding))(server.pwfs1Follow),
    command("pwfs1Unwrap")(server.pwfs1Unwrap),
    command("pwfs1Observe", arg("period", TimeSpanInput.Binding))(server.pwfs1Observe),
    command("pwfs1StopObserve")(server.pwfs1StopObserve),
    command("pwfs1Filter", arg("filter", PwfsFilterBinding))(server.pwfs1Filter),
    command("pwfs1FieldStop", arg("fieldStop", PwfsFieldStopBinding))(server.pwfs1FieldStop),
    command("pwfs1CircularBuffer", arg("enable", BooleanBinding))(server.pwfs1CircularBuffer),
    command("pwfs1QlMode", arg("mode", QlModeBinding))(server.pwfs1QlMode),
    // PWFS2
    command("pwfs2Target", arg("target", TargetPropertiesInput.Binding))(server.pwfs2Target),
    command("pwfs2ProbeTracking", arg("config", ProbeTrackingInput.Binding))(
      server.pwfs2ProbeTracking
    ),
    command("pwfs2Park")(server.pwfs2Park),
    command("pwfs2Follow", arg("enable", BooleanBinding))(server.pwfs2Follow),
    command("pwfs2Unwrap")(server.pwfs2Unwrap),
    command("pwfs2Observe", arg("period", TimeSpanInput.Binding))(server.pwfs2Observe),
    command("pwfs2StopObserve")(server.pwfs2StopObserve),
    command("pwfs2Filter", arg("filter", PwfsFilterBinding))(server.pwfs2Filter),
    command("pwfs2FieldStop", arg("fieldStop", PwfsFieldStopBinding))(server.pwfs2FieldStop),
    command("pwfs2CircularBuffer", arg("enable", BooleanBinding))(server.pwfs2CircularBuffer),
    command("pwfs2QlMode", arg("mode", QlModeBinding))(server.pwfs2QlMode),
    // OIWFS
    command("oiwfsTarget", arg("target", TargetPropertiesInput.Binding))(server.oiwfsTarget),
    command("oiwfsProbeTracking", arg("config", ProbeTrackingInput.Binding))(
      server.oiwfsProbeTracking
    ),
    command("oiwfsPark")(server.oiwfsPark),
    command("oiwfsFollow", arg("enable", BooleanBinding))(server.oiwfsFollow),
    command("oiwfsObserve", arg("period", TimeSpanInput.Binding))(server.oiwfsObserve),
    command("oiwfsStopObserve")(server.oiwfsStopObserve),
    command("oiwfsCircularBuffer", arg("enable", BooleanBinding))(server.oiwfsCircularBuffer),
    command("oiwfsQlMode", arg("mode", QlModeBinding))(server.oiwfsQlMode),
    // AC
    command("acObserve", arg("period", TimeSpanInput.Binding))(server.acObserve),
    command("acStopObserve")(server.acStopObserve),
    command("acLens", arg("lens", AcLensBinding))(server.acLens),
    command("acFilter", arg("filter", AcFilterBinding))(server.acFilter),
    command("acNdFilter", arg("ndFilter", AcNdFilterBinding))(server.acNdFilter),
    command("acWindowSize", arg("size", AcWindowInput.Binding))(server.acWindowSize),
    // Guiding
    command("guideEnable", arg("config", GuideConfigurationInput.Binding))(server.enableGuide),
    command("guideDisable")(server.disableGuide),
    command("wfsSky", args("wfs", GuideProbeBinding, "period", TimeSpanInput.Binding))(
      (wfs, period) => server.wfsSky(wfs, period)
    ),
    // M1
    command("m1Park")(server.m1Park),
    command("m1Unpark")(server.m1Unpark),
    command("m1OpenLoopOff")(server.m1OpenLoopOff),
    command("m1OpenLoopOn")(server.m1OpenLoopOn),
    command("m1ZeroFigure")(server.m1ZeroFigure),
    command("m1LoadAoFigure")(server.m1LoadAoFigure),
    command("m1LoadNonAoFigure")(server.m1LoadNonAoFigure),
    // Light path and step configuration
    command(
      "lightpathConfig",
      {
        case List(
              LightSourceBinding("from", rFrom),
              InstrumentBinding("instrument", rInstrument),
              LightSinkVariantBinding.Option("lightSinkVariant", rLightSinkVariant)
            ) =>
          (rFrom, LightPathInput.lightSink(rInstrument, rLightSinkVariant)).parTupled
      }
    )((from, lightSink) => server.lightPathConfig(from, lightSink)),
    command("offset", args("offset", OffsetInput.Binding, "guiding", BooleanBinding))(
      (offset, guiding) => server.offset(offset, guiding)
    ),
    command("centralWavelength", arg("wavelength", WavelengthInput.Binding))(
      server.centralWavelength
    ),
    command("configureStep", arg("config", ConfigureStepInput.Binding)): c =>
      server.configureStep(c.offset, c.wavelength, c.lightPath, c.defocus, c.guiding),
    // Adjustments
    rootField(
      MutationType,
      "acquisitionAdjustment",
      arg("adjustment", AcquisitionAdjustmentInput.Binding)
    ): adj =>
      // Publish first. Other clients are informed even if the action fails.
      topics.acquisitionAdjustment.publish1(adj) *>
        // If the user confirms, run the adjustment. Keep the upstream error.
        (adj.command === AcquisitionAdjustmentCommand.UserConfirms)
          .valueOrPure[F, Result[OperationOutcome]](
            server.acquisitionAdj(adj.offset, adj.iaa, adj.ipa).attemptResultOutcome
          )(OperationOutcome.success.success),
    command(
      "adjustTarget",
      args(
        "target",
        VirtualTelescopeBinding,
        "offset",
        HandsetAdjustmentInput.Binding,
        "openLoops",
        BooleanBinding
      )
    )((target, offset, openLoops) => server.targetAdjust(target, offset, openLoops)),
    command("adjustPointing", arg("offset", HandsetAdjustmentInput.Binding))(
      server.pointingAdjust
    ),
    command(
      "adjustOrigin",
      args("offset", HandsetAdjustmentInput.Binding, "openLoops", BooleanBinding)
    )((offset, openLoops) => server.originAdjust(offset, openLoops)),
    command(
      "resetTargetAdjustment",
      args("target", VirtualTelescopeBinding, "openLoops", BooleanBinding)
    )((target, openLoops) => server.targetOffsetClear(target, openLoops)),
    command("absorbTargetAdjustment", arg("target", VirtualTelescopeBinding))(
      server.targetOffsetAbsorb
    ),
    command("resetLocalPointingAdjustment")(server.pointingOffsetClearLocal),
    command("resetGuidePointingAdjustment")(server.pointingOffsetClearGuide),
    command("absorbGuidePointingAdjustment")(server.pointingOffsetAbsorbGuide),
    command("resetOriginAdjustment", arg("openLoops", BooleanBinding))(server.originOffsetClear),
    command("absorbOriginAdjustment")(server.originOffsetAbsorb),
    command("refreshEphemerisFiles", arg("observingNight", DateBinding.Option))(
      server.refreshEphemerides
    ),
    // AG
    command("agScienceFoldPark")(server.agScienceFoldPark),
    command("agPickoffMirrorPark")(server.agPickoffMirrorPark),
    command("agAoFoldPark")(server.agAoFoldPark),
    command("agAllPark")(server.agAllPark),
    // ECS
    command("ecsEnableDome", arg("mode", DomeModeBinding))(server.ecsEnableDome),
    command("ecsDisableDome")(server.ecsDisableDome),
    command("ecsDomePark")(server.ecsDomePark),
    command("ecsEnableShutters", arg("mode", ShutterModeInput.Binding))(server.ecsEnableShutters),
    command("ecsDisableShutters")(server.ecsDisableShutters),
    command("ecsShuttersPark")(server.ecsShuttersPark),
    command("ecsMoveEastVentGate", arg("position", IntPercentBinding))(server.ecsMoveEastVentGate),
    command("ecsCloseEastVentGate")(server.ecsCloseEastVentGate),
    command("ecsMoveWestVentGate", arg("position", IntPercentBinding))(server.ecsMoveWestVentGate),
    command("ecsCloseWestVentGate")(server.ecsCloseWestVentGate)
  )

  override val selectElaborator: SelectElaborator = SelectElaborator(
    (queryFields ++ mutationFields).map(_.elaborator).reduce(_ orElse _)
  )

  override val typeMappings: TypeMappings = TypeMappings(
    List(
      ObjectMapping(tpe = QueryType, fieldMappings = queryFields.map(_.fieldMapping)),
      ObjectMapping(tpe = MutationType, fieldMappings = mutationFields.map(_.fieldMapping)),
      ObjectMapping(
        tpe = SubscriptionType,
        List(
          RootStream.computeEncodable("logMessage") { (_, _) =>
            (Stream.evalSeq(topics.logBuffer.get) ++
              topics.loggingEvents.subscribe(1024))
              .map(_.success)
          },
          RootStream.computeEncodable("guideState") { (_, _) =>
            topics.guideState
              .subscribe(1024)
              .map(_.success)
          },
          RootStream.computeEncodable("guidersQualityValues") { (_, _) =>
            topics.guidersQuality
              .subscribe(1024)
              .map(_.success)
          },
          RootStream.computeEncodable("telescopeState") { (_, _) =>
            topics.telescopeState
              .subscribe(1024)
              .map(_.success)
          },
          RootStream.computeEncodable("navigateState") { (_, _) =>
            server.getNavigateStateStream
              .map(_.success)
          },
          RootStream.computeEncodable("acquisitionAdjustmentState") { (_, _) =>
            topics.acquisitionAdjustment
              .subscribe(1024)
              .map(_.success)
          },
          RootStream.computeEncodable("targetAdjustmentOffsets") { (_, _) =>
            topics.targetAdjustment
              .subscribe(1024)
              .map(_.success)
          },
          RootStream.computeEncodable("originAdjustmentOffset") { (_, _) =>
            topics.originAdjustment
              .subscribe(1024)
              .map(_.success)
          },
          RootStream.computeEncodable("pointingAdjustmentOffset") { (_, _) =>
            topics.pointingAdjustment
              .subscribe(1024)
              .map(_.success)
          },
          RootStream.computeEncodable("acMechsState") { (_, _) =>
            topics.acMechsState
              .subscribe(1024)
              .map(_.success)
          },
          RootStream.computeEncodable("pwfs1MechsState") { (_, _) =>
            topics.pwfs1MechsTopic
              .subscribe(1024)
              .map(_.success)
          },
          RootStream.computeEncodable("pwfs2MechsState") { (_, _) =>
            topics.pwfs2MechsTopic
              .subscribe(1024)
              .map(_.success)
          },
          RootStream.computeEncodable("pwfs1ConfigState") { (_, _) =>
            topics.pwfs1WfsTopic
              .subscribe(1024)
              .map(_.success)
          },
          RootStream.computeEncodable("pwfs2ConfigState") { (_, _) =>
            topics.pwfs2WfsTopic
              .subscribe(1024)
              .map(_.success)
          },
          RootStream.computeEncodable("oiwfsConfigState") { (_, _) =>
            topics.oiwfsWfsTopic
              .subscribe(1024)
              .map(_.success)
          }
        )
      )
    )
  )
}

object NavigateMappings {

  /** Environment key for the parsed arguments of a root field. */
  private val ArgsKey = "args"

  def loadSchema[F[_]: {Sync, Logger}]: F[Schema] =
    SchemaStitcher.load("navigate.graphql")

  def apply[F[_]: {Sync, Logger}](
    config: NavigateConfiguration,
    server: NavigateEngine[F],
    topics: TopicManager[F]
  ): F[NavigateMappings[F]] =
    loadSchema[F]
      .map(
        new NavigateMappings[F](
          config,
          server,
          topics
        )(_)
      )

  extension [F[_]: MonadThrow, A](fa: F[A])
    def attemptResult: F[Result[A]] =
      fa.attempt.map(e => Result.fromEither(e.leftMap(_.getMessage)))

  extension [F[_]: MonadThrow](fa: F[CommandResult])
    def attemptResultOutcome: F[Result[OperationOutcome]] =
      fa.attempt.map {
        case Right(CommandResult.CommandSuccess)      => OperationOutcome.success.success
        case Right(CommandResult.CommandPaused)       => OperationOutcome.success.success
        case Right(CommandResult.CommandFailure(msg)) => Result.failure(msg)
        case Left(e)                                  => Result.internalError(e)
      }

}
