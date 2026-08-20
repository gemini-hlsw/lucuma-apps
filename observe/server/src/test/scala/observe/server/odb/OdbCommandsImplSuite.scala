// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.effect.IO
import cats.effect.Ref
import cats.effect.std.Mutex
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import clue.FetchClientWithPars
import clue.ResponseException
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import lucuma.refined.*
import lucuma.schemas.ObservationDB
import munit.CatsEffectSuite
import observe.model.dhs.ImageFileId
import observe.model.odb.ObsRecordedIds
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.syntax.all.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

import java.util.UUID

class OdbCommandsImplSuite extends CatsEffectSuite:

  private given Logger[IO] = NoOpLogger.impl[IO]

  private val obsId  = Observation.Id(1L.refined)
  private val stepId = Step.Id.fromUuid(UUID.fromString("00000000-0000-0000-0000-000000000001"))
  private val fileId = ImageFileId("S20260820S0001")

  private case class Recorded(op: String, variables: Json)

  private def opName(query: String): String =
    List(
      "addEventBatch",
      "addSequenceEvent",
      "addStepEvent",
      "addDatasetEvent",
      "recordDataset",
      "recordVisit"
    ).find(query.contains).getOrElse("unknown")

  private def cannedResponse(op: String): Json = op match
    case "recordVisit"      => json"""{"data": {"recordVisit": {"visit": {"id": "v-64"}}}}"""
    case "recordDataset"    =>
      json"""{"data": {"recordDataset": {"dataset": {"id": "d-65", "reference": null}}}}"""
    case "addStepEvent"     => json"""{"data": {"addStepEvent": {"event": {"id": "e-1"}}}}"""
    case "addDatasetEvent"  => json"""{"data": {"addDatasetEvent": {"event": {"id": "e-2"}}}}"""
    case "addSequenceEvent" =>
      json"""{"data": {"addSequenceEvent": {"event": {"recordedTime": "2026-01-01 00:00:00"}}}}"""
    case "addEventBatch"    =>
      json"""{"data": {"addEventBatch": {"events": [{"id": "e-3"}], "hasMore": false}}}"""
    case _                  => json"""{"errors": [{"message": "unknown operation"}]}"""

  /**
   * Fake ODB: records every GraphQL request, then answers with canned data. `batchTransportFails`
   * makes that many addEventBatch requests fail at the HTTP level; `batchGraphQLRejects` makes
   * addEventBatch return a GraphQL error response instead.
   */
  private def fakeOdb(
    recorded:            Ref[IO, Vector[Recorded]],
    batchTransportFails: Ref[IO, Int],
    batchGraphQLRejects: Boolean
  ): HttpApp[IO] =
    HttpApp[IO]: req =>
      for
        body    <- req.as[Json]
        query    = body.hcursor.get[String]("query").getOrElse("")
        vars     = body.hcursor.downField("variables").focus.getOrElse(Json.Null)
        op       = opName(query)
        _       <- recorded.update(_ :+ Recorded(op, vars))
        failNow <-
          if (op === "addEventBatch")
            batchTransportFails.modify(n => (n - 1).max(0) -> (n > 0))
          else false.pure[IO]
      yield
        if (failNow) Response[IO](Status.InternalServerError)
        else if (op === "addEventBatch" && batchGraphQLRejects)
          Response[IO](Status.Ok).withEntity(json"""{"errors": [{"message": "rejected"}]}""")
        else Response[IO](Status.Ok).withEntity(cannedResponse(op))

  private def mkCommands(
    batching: Boolean,
    app:      HttpApp[IO]
  ): IO[OdbCommandsImpl[IO]] =
    given Http4sHttpBackend[IO] = Http4sHttpBackend(Client.fromHttpApp(app))
    for
      client        <- Http4sHttpClient.of[IO, ObservationDB](uri"http://odb", "ODB")
      idTracker     <- Ref.of[IO, ObsRecordedIds](ObsRecordedIds.Empty)
      pendingEvents <- Ref.of[IO, PendingEvents](Map.empty)
      flushMutex    <- Mutex[IO]
    yield
      given FetchClientWithPars[IO, Request[IO], ObservationDB] = client
      OdbCommandsImpl[IO](idTracker, batching, pendingEvents, flushMutex)

  private def setup(
    batching:            Boolean,
    batchTransportFails: Int = 0,
    batchGraphQLRejects: Boolean = false
  ): IO[(OdbCommandsImpl[IO], Ref[IO, Vector[Recorded]])] =
    for
      recorded <- Ref.of[IO, Vector[Recorded]](Vector.empty)
      fails    <- Ref.of[IO, Int](batchTransportFails)
      cmds     <- mkCommands(batching, fakeOdb(recorded, fails, batchGraphQLRejects))
    yield (cmds, recorded)

  private def ops(recorded: Ref[IO, Vector[Recorded]]): IO[List[String]] =
    recorded.get.map(_.map(_.op).toList)

  /** The full event traffic of one step with one dataset. */
  private def runFullStep(cmds: OdbCommandsImpl[IO]): IO[Unit] =
    for
      _ <- cmds.visitStart(obsId)
      _ <- cmds.stepStartStep(obsId, stepId)
      _ <- cmds.stepStartConfigure(obsId, stepId)
      _ <- cmds.stepEndConfigure(obsId, stepId)
      _ <- cmds.stepStartObserve(obsId, stepId)
      _ <- cmds.datasetStartExposure(obsId, stepId, fileId)
      _ <- cmds.datasetEndExposure(obsId, fileId)
      _ <- cmds.datasetStartReadout(obsId, fileId)
      _ <- cmds.datasetEndReadout(obsId, fileId)
      _ <- cmds.datasetStartWrite(obsId, fileId)
      _ <- cmds.datasetEndWrite(obsId, fileId)
      _ <- cmds.stepEndObserve(obsId, stepId)
      _ <- cmds.stepEndStep(obsId, stepId)
    yield ()

  /** Extracts (kind, stage) for each entry of a recorded addEventBatch request. */
  private def batchEntries(vars: Json): List[(String, String)] =
    vars.hcursor
      .downField("input")
      .downField("events")
      .focus
      .flatMap(_.asArray)
      .getOrElse(Vector.empty)
      .toList
      .map: entry =>
        val c = entry.hcursor
        c.get[Json]("step")
          .toOption
          .map(s => "step" -> s.hcursor.get[String]("stepStage").getOrElse(""))
          .orElse(
            c.get[Json]("dataset")
              .toOption
              .map(d => "dataset" -> d.hcursor.get[String]("datasetStage").getOrElse(""))
          )
          .getOrElse("unknown" -> "")

  private def batchIdempotencyKeys(vars: Json): List[String] =
    vars.hcursor
      .downField("input")
      .downField("events")
      .focus
      .flatMap(_.asArray)
      .getOrElse(Vector.empty)
      .toList
      .flatMap: entry =>
        val c = entry.hcursor
        c.downField("step")
          .get[String]("idempotencyKey")
          .orElse(c.downField("dataset").get[String]("idempotencyKey"))
          .toOption

  test("flag off: a full step sends only per-event mutations, never a batch"):
    for
      (cmds, recorded) <- setup(batching = false)
      _                <- runFullStep(cmds)
      opList           <- ops(recorded)
    yield
      assertEquals(
        opList,
        List(
          "recordVisit",
          "addStepEvent",   // StartStep
          "addStepEvent",   // StartConfigure
          "addStepEvent",   // EndConfigure
          "addStepEvent",   // StartObserve
          "recordDataset",
          "addDatasetEvent", // StartExpose
          "addDatasetEvent", // EndExpose
          "addDatasetEvent", // StartReadout
          "addDatasetEvent", // EndReadout
          "addDatasetEvent", // StartWrite
          "addDatasetEvent", // EndWrite
          "addStepEvent",   // EndObserve
          "addStepEvent"    // EndStep
        )
      )
      assert(!opList.contains("addEventBatch"))

  test("flag on: START_STEP is sent synchronously, before anything buffers"):
    for
      (cmds, recorded) <- setup(batching = true)
      _                <- cmds.visitStart(obsId)
      _                <- cmds.stepStartStep(obsId, stepId)
      opList           <- ops(recorded)
    yield assertEquals(opList, List("recordVisit", "addStepEvent"))

  test("flag on: intermediate events buffer without any request"):
    for
      (cmds, recorded) <- setup(batching = true)
      _                <- cmds.visitStart(obsId)
      _                <- cmds.stepStartStep(obsId, stepId)
      before           <- ops(recorded)
      _                <- cmds.stepStartConfigure(obsId, stepId)
      _                <- cmds.stepEndConfigure(obsId, stepId)
      _                <- cmds.stepStartObserve(obsId, stepId)
      after            <- ops(recorded)
    yield assertEquals(after, before)

  test("flag on: recordDataset stays synchronous and only StartExpose buffers"):
    for
      (cmds, recorded) <- setup(batching = true)
      _                <- cmds.visitStart(obsId)
      _                <- cmds.stepStartStep(obsId, stepId)
      dataset          <- cmds.datasetStartExposure(obsId, stepId, fileId)
      opList           <- ops(recorded)
    yield
      assertEquals(dataset.id.toString, "d-65")
      assertEquals(opList, List("recordVisit", "addStepEvent", "recordDataset"))

  test("flag on: END_STEP flushes one atomic ordered batch with distinct keys"):
    for
      (cmds, recorded) <- setup(batching = true)
      _                <- runFullStep(cmds)
      all              <- recorded.get
      opList            = all.map(_.op).toList
      batches           = all.filter(_.op === "addEventBatch")
    yield
      assertEquals(
        opList,
        List("recordVisit", "addStepEvent", "recordDataset", "addEventBatch")
      )
      assertEquals(batches.size, 1)
      val entries = batchEntries(batches.head.variables)
      assertEquals(
        entries,
        List(
          "step"    -> "START_CONFIGURE",
          "step"    -> "END_CONFIGURE",
          "step"    -> "START_OBSERVE",
          "dataset" -> "START_EXPOSE",
          "dataset" -> "END_EXPOSE",
          "dataset" -> "START_READOUT",
          "dataset" -> "END_READOUT",
          "dataset" -> "START_WRITE",
          "dataset" -> "END_WRITE",
          "step"    -> "END_OBSERVE",
          "step"    -> "END_STEP"
        )
      )
      val keys = batchIdempotencyKeys(batches.head.variables)
      assertEquals(keys.size, entries.size)
      assertEquals(keys.distinct.size, keys.size)

  test("flag on: sequence events flush the buffer first, then send"):
    for
      (cmds, recorded) <- setup(batching = true)
      _                <- cmds.visitStart(obsId)
      _                <- cmds.stepStartStep(obsId, stepId)
      _                <- cmds.stepStartConfigure(obsId, stepId)
      _                <- cmds.obsPause(obsId)
      opList           <- ops(recorded)
    yield assertEquals(
      opList,
      List("recordVisit", "addStepEvent", "addEventBatch", "addSequenceEvent")
    )

  test("flag on: sequence event with empty buffer sends no batch"):
    for
      (cmds, recorded) <- setup(batching = true)
      _                <- cmds.visitStart(obsId)
      _                <- cmds.obsContinue(obsId)
      opList           <- ops(recorded)
    yield assertEquals(opList, List("recordVisit", "addSequenceEvent"))

  test("flag on: step abort appends the event and flushes immediately"):
    for
      (cmds, recorded) <- setup(batching = true)
      _                <- cmds.visitStart(obsId)
      _                <- cmds.stepStartStep(obsId, stepId)
      _                <- cmds.stepStartConfigure(obsId, stepId)
      _                <- cmds.stepAbort(obsId, stepId)
      all              <- recorded.get
      batches           = all.filter(_.op === "addEventBatch")
    yield
      assertEquals(batches.size, 1)
      assertEquals(
        batchEntries(batches.head.variables),
        List("step" -> "START_CONFIGURE", "step" -> "ABORT")
      )

  test("flag on: transport failure retries with identical payload, then succeeds"):
    TestControl.executeEmbed(
      for
        (cmds, recorded) <- setup(batching = true, batchTransportFails = 2)
        _                <- cmds.visitStart(obsId)
        _                <- cmds.stepStartStep(obsId, stepId)
        _                <- cmds.stepStartConfigure(obsId, stepId)
        _                <- cmds.stepEndStep(obsId, stepId)
        all              <- recorded.get
        batches           = all.filter(_.op === "addEventBatch")
      yield
        assertEquals(batches.size, 3)
        assertEquals(batches.map(_.variables).distinct.size, 1)
    )

  test("flag on: retry exhaustion raises and re-buffers; next flush re-attempts the events"):
    TestControl.executeEmbed(
      for
        (cmds, recorded) <- setup(batching = true, batchTransportFails = 5)
        _                <- cmds.visitStart(obsId)
        _                <- cmds.stepStartStep(obsId, stepId)
        _                <- cmds.stepStartConfigure(obsId, stepId)
        result           <- cmds.stepEndStep(obsId, stepId).attempt
        _                <- cmds.obsPause(obsId)
        all              <- recorded.get
        batches           = all.filter(_.op === "addEventBatch")
      yield
        assert(result.isLeft)
        // 5 failed attempts (initial + 4 retries), then the obsPause flush succeeds.
        assertEquals(batches.size, 6)
        assertEquals(batches.map(_.variables).distinct.size, 1)
        assertEquals(
          batchEntries(batches.last.variables),
          List("step" -> "START_CONFIGURE", "step" -> "END_STEP")
        )
    )

  test("flag on: a GraphQL rejection raises without retry and drops the events"):
    for
      (cmds, recorded) <- setup(batching = true, batchGraphQLRejects = true)
      _                <- cmds.visitStart(obsId)
      _                <- cmds.stepStartStep(obsId, stepId)
      _                <- cmds.stepStartConfigure(obsId, stepId)
      result           <- cmds.stepEndStep(obsId, stepId).attempt
      _                <- cmds.obsPause(obsId).attempt.void
      all              <- recorded.get
      batches           = all.filter(_.op === "addEventBatch")
    yield
      assert(result.left.exists(_.isInstanceOf[ResponseException[?]]))
      assertEquals(batches.size, 1)

  test("flag on: obsStop clears the current visit"):
    for
      (cmds, recorded) <- setup(batching = true)
      _                <- cmds.visitStart(obsId)
      _                <- cmds.obsStop(obsId)
      ids              <- cmds.getCurrentRecordedIds
    yield assertEquals(ObsRecordedIds.at(obsId).get(ids), None)
