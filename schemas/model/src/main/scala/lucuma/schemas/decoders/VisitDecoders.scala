// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.decoders

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosShort
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Json
import io.circe.refined.given
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.json.flamingos2.given
import lucuma.odb.json.ghost.given
import lucuma.odb.json.gmos.given
import lucuma.odb.json.igrins2.given
import lucuma.odb.json.stepconfig.given
import lucuma.odb.json.time.decoder.given
import lucuma.schemas.model.*
import lucuma.schemas.model.enums.AtomExecutionState
import lucuma.schemas.model.enums.StepExecutionState

trait VisitDecoders:
  given Decoder[Dataset.Filename] = Decoder.instance: c =>
    c.as[String]
      .flatMap:
        Dataset.Filename
          .parse(_)
          .toRight(DecodingFailure("Error parsing Dataset.Filename", c.history))

  given Decoder[Dataset] = Decoder.instance: c =>
    for
      id        <- c.downField("id").as[Dataset.Id]
      index     <- c.downField("index").as[PosShort]
      filename  <- c.downField("filename").as[Dataset.Filename]
      qaState   <- c.downField("qaState").as[Option[DatasetQaState]]
      comment   <- c.downField("comment").as[Option[NonEmptyString]]
      interval  <- c.downField("interval").as[Option[TimestampInterval]]
      isWritten <- c.downField("isWritten").as[Boolean]
    yield Dataset(id, index, filename, qaState, comment, interval, isWritten)

  // Generic step record decoder builder.
  private def stepRecordDecoder[D: Decoder, R <: StepRecord[D]](
    configField:     String,
    buildStepRecord: (
      Step.Id,
      StepExecutionState,
      Option[TimestampInterval],
      D,
      StepConfig,
      TelescopeConfig,
      ObserveClass,
      Option[DatasetQaState],
      List[Dataset]
    ) => R
  ): Decoder[R] = Decoder.instance: c =>
    for
      id              <- c.downField("id").as[Step.Id]
      executionState  <- c.downField("executionState").as[StepExecutionState]
      interval        <- c.downField("interval").as[Option[TimestampInterval]]
      instrumentConf  <- c.downField(configField).as[D]
      stepConfig      <- c.downField("stepConfig").as[StepConfig]
      telescopeConfig <- c.downField("telescopeConfig").as[TelescopeConfig]
      observeClass    <- c.downField("observeClass").as[ObserveClass]
      qaState         <- c.downField("qaState").as[Option[DatasetQaState]]
      datasets        <- c.downField("datasets").downField("matches").as[List[Dataset]]
    yield buildStepRecord(
      id,
      executionState,
      interval,
      instrumentConf,
      stepConfig,
      telescopeConfig,
      observeClass,
      qaState,
      datasets
    )

  given Decoder[StepRecord.GmosNorth] =
    stepRecordDecoder[gmos.DynamicConfig.GmosNorth, StepRecord.GmosNorth](
      "gmosNorth",
      StepRecord.GmosNorth.apply
    )

  given Decoder[StepRecord.GmosSouth] =
    stepRecordDecoder[gmos.DynamicConfig.GmosSouth, StepRecord.GmosSouth](
      "gmosSouth",
      StepRecord.GmosSouth.apply
    )

  given Decoder[StepRecord.Flamingos2] =
    stepRecordDecoder[Flamingos2DynamicConfig, StepRecord.Flamingos2](
      "flamingos2",
      StepRecord.Flamingos2.apply
    )

  given Decoder[StepRecord.Igrins2] =
    stepRecordDecoder[Igrins2DynamicConfig, StepRecord.Igrins2]("igrins2", StepRecord.Igrins2.apply)

  given Decoder[StepRecord.Ghost] =
    stepRecordDecoder[GhostDynamicConfig, StepRecord.Ghost]("ghost", StepRecord.Ghost.apply)

  // Generic atom decoder builder.
  private def atomRecordDecoder[S: Decoder, R](
    buildAtom: (Atom.Id, AtomExecutionState, Option[TimestampInterval], SequenceType, List[S]) => R
  ): Decoder[R] = Decoder.instance: c =>
    for
      id             <- c.downField("id").as[Atom.Id]
      executionState <- c.downField("executionState").as[AtomExecutionState]
      interval       <- c.downField("interval").as[Option[TimestampInterval]]
      sequenceType   <- c.downField("sequenceType").as[SequenceType]
      steps          <- c.downField("steps").downField("matches").as[List[S]]
    yield buildAtom(id, executionState, interval, sequenceType, steps)

  // We must specify a name since the automatic names only take the last part of the type path,
  // generating conflicts among all the `.GmosNorth` and `.GmosSouth` types.
  // See https://dotty.epfl.ch/docs/reference/contextual/givens.html#anonymous-givens
  given decoderAtomGmosNorth: Decoder[AtomRecord.GmosNorth]   =
    atomRecordDecoder(AtomRecord.GmosNorth.apply)
  given decoderAtomGmosSouth: Decoder[AtomRecord.GmosSouth]   =
    atomRecordDecoder(AtomRecord.GmosSouth.apply)
  given decoderAtomFlamingos2: Decoder[AtomRecord.Flamingos2] =
    atomRecordDecoder(AtomRecord.Flamingos2.apply)
  given decoderAtomIgrins2: Decoder[AtomRecord.Igrins2]       =
    atomRecordDecoder(AtomRecord.Igrins2.apply)
  given decoderAtomGhost: Decoder[AtomRecord.Ghost]           =
    atomRecordDecoder(AtomRecord.Ghost.apply)

  private def visitDecoder[A: Decoder, R](
    expected:   Instrument,
    buildVisit: (Visit.Id, Timestamp, Option[TimestampInterval], List[A]) => R
  ): Decoder[R] = Decoder.instance: c =>
    for
      instrument <- c.downField("instrument").as[Instrument]
      _          <- Either.cond(instrument === expected,
                                (),
                                DecodingFailure(
                                  s"Attempted to decode a $instrument visit in $expected decoder",
                                  c.history
                                )
                    )
      id         <- c.downField("id").as[Visit.Id]
      created    <- c.downField("created").as[Timestamp]
      interval   <- c.downField("interval").as[Option[TimestampInterval]]
      atoms      <- c.downField("atomRecords").downField("matches").as[List[A]]
    yield buildVisit(id, created, interval, atoms)

  given decoderVisitGmosNorth: Decoder[Visit.GmosNorth]   =
    visitDecoder(Instrument.GmosNorth, Visit.GmosNorth.apply)
  given decoderVisitGmosSouth: Decoder[Visit.GmosSouth]   =
    visitDecoder(Instrument.GmosSouth, Visit.GmosSouth.apply)
  given decoderVisitFlamingos2: Decoder[Visit.Flamingos2] =
    visitDecoder(Instrument.Flamingos2, Visit.Flamingos2.apply)
  given decoderVisitIgrins2: Decoder[Visit.Igrins2]       =
    visitDecoder(Instrument.Igrins2, Visit.Igrins2.apply)
  given decoderVisitGhost: Decoder[Visit.Ghost]           =
    visitDecoder(Instrument.Ghost, Visit.Ghost.apply)

  private def executionVisitsDecoder[V: Decoder, R](
    build: NonEmptyList[V] => R
  ): Decoder[R] = Decoder.instance: c =>
    c.downField("visits").downField("matches").as[NonEmptyList[V]].map(build)

  given decoderExecutionVisitsGmosNorth: Decoder[ExecutionVisits.GmosNorth]   =
    executionVisitsDecoder(ExecutionVisits.GmosNorth.apply)
  given decoderExecutionVisitsGmosSouth: Decoder[ExecutionVisits.GmosSouth]   =
    executionVisitsDecoder(ExecutionVisits.GmosSouth.apply)
  given decoderExecutionVisitsFlamingos2: Decoder[ExecutionVisits.Flamingos2] =
    executionVisitsDecoder(ExecutionVisits.Flamingos2.apply)
  given decoderExecutionVisitsIgrins2: Decoder[ExecutionVisits.Igrins2]       =
    executionVisitsDecoder(ExecutionVisits.Igrins2.apply)
  given decoderExecutionVisitsGhost: Decoder[ExecutionVisits.Ghost]           =
    executionVisitsDecoder(ExecutionVisits.Ghost.apply)

  val decoderNoVisits: Decoder[Option[ExecutionVisits]] = Decoder.instance: c =>
    c.downField("visits")
      .downField("matches")
      .as[List[Json]]
      .flatMap: list =>
        if list.isEmpty then Right(none)
        else Left(DecodingFailure("Expected no visits but found some", c.history))

  given Decoder[Option[ExecutionVisits]] =
    List(
      Decoder[ExecutionVisits.GmosNorth].widen,
      Decoder[ExecutionVisits.GmosSouth].widen,
      Decoder[ExecutionVisits.Flamingos2].widen,
      Decoder[ExecutionVisits.Igrins2].widen,
      Decoder[ExecutionVisits.Ghost].widen
    ).reduceLeft(_ or _)
      .map(_.some)
      .or(decoderNoVisits)
