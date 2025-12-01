// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.*
import cats.derived.*
import cats.syntax.all.*
import explore.model.ProgramTime
import io.circe.Decoder
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

case class Execution(
  digest:            CalculatedValue[Option[ExecutionDigest]],
  programTimeCharge: ProgramTime
) derives Eq:
  def acqOffset = digest.value.foldMap(_.acquisition.configs.map(_.offset))
  def sciOffset = digest.value.foldMap(_.science.configs.map(_.offset))

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

  given Decoder[Execution] = Decoder.instance(c =>
    for {
      d  <- c.get[CalculatedValue[Option[ExecutionDigest]]]("digest")
      pt <- c.get[ProgramTime]("timeCharge")
    } yield Execution(d, pt)
  )
