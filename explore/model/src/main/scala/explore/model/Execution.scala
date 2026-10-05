// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.catsKernelOrderingForOrder
import cats.derived.*
import cats.syntax.all.*
import explore.model.ProgramTime
import io.circe.Decoder
import lucuma.core.enums.CloneSequenceMode
import lucuma.core.enums.ExecutionState
import lucuma.core.math.Offset
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.util.CalculatedValue
import lucuma.core.util.TimeSpan
import lucuma.odb.json.sequence.given
import lucuma.schemas.decoders.given
import monocle.Focus
import monocle.Lens
import monocle.Optional

import scala.collection.immutable.SortedSet

final case class Execution(
  digest:                            CalculatedValue[Option[ExecutionDigest]],
  programTimeCharge:                 ProgramTime,
  originalEstimate:                  Option[ProgramTime],
  acquisitionSequenceIsMaterialized: Boolean,
  scienceSequenceIsMaterialized:     Boolean,
  executionState:                    ExecutionState
) derives Eq:
  val hasMaterializedSequence: Boolean =
    acquisitionSequenceIsMaterialized || scienceSequenceIsMaterialized

  // The Sequence Copy choices for a Duplicate, empty when there is nothing to ask. Pending and all
  // steps are the same when nothing has started, so only all steps is offered then. The ODB never
  // returns to NotDefined once execution starts.
  lazy val sequenceCopyChoices: List[CloneSequenceMode] =
    if !hasMaterializedSequence then Nil
    else if executionState === ExecutionState.NotStarted ||
      executionState === ExecutionState.NotDefined
    then
      List(CloneSequenceMode.None, CloneSequenceMode.AllSteps)
    else List(CloneSequenceMode.None, CloneSequenceMode.PendingSteps, CloneSequenceMode.AllSteps)

  lazy val acqOffset: SortedSet[Offset] =
    digest.value.foldMap(_.acquisition.telescopeConfigs.map(_.offset))
  lazy val sciOffset: SortedSet[Offset] =
    digest.value.foldMap(_.science.telescopeConfigs.map(_.offset))

object Execution:
  val digest: Lens[Execution, CalculatedValue[Option[ExecutionDigest]]] =
    Focus[Execution](_.digest)

  val programTimeCharge: Lens[Execution, ProgramTime] =
    Focus[Execution](_.programTimeCharge)

  val sciConfigs: Optional[Execution, SortedSet[TelescopeConfig]] =
    digest
      .andThen(CalculatedValue.value.some)
      .andThen(ExecutionDigest.science.andThen(SequenceDigest.configs))

  val acqConfigs: Optional[Execution, SortedSet[TelescopeConfig]] =
    digest
      .andThen(CalculatedValue.value.some)
      .andThen(ExecutionDigest.acquisition.andThen(SequenceDigest.configs))

  given Decoder[Execution] = Decoder.instance: c =>
    for
      d  <- c.get[CalculatedValue[Option[ExecutionDigest]]]("digest")
      pt <- c.get[ProgramTime]("timeCharge")
      oe <- c.get[Option[ProgramTime]]("originalEstimate")(using
              Decoder.decodeOption(using Decoder.instance(_.get[ProgramTime]("total")))
            )
      a  <- c.get[Boolean]("acquisitionSequenceIsMaterialized")
      s  <- c.get[Boolean]("scienceSequenceIsMaterialized")
      es <- c.get[ExecutionState]("executionState")
    yield Execution(d, pt, oe, a, s, es)
