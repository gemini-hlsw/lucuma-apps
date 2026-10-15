// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.services

import cats.effect.IO
import cats.effect.Sync
import japgolly.scalajs.react.React
import japgolly.scalajs.react.feature.Context
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.Step
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.Visit
import lucuma.ui.sequence.SequenceData
import observe.model.Observation

import scala.annotation.unused

trait ODBQueryApi[F[_]: Sync]:
  def queryVisits(
    @unused obsId: Observation.Id,
    @unused from:  Option[Visit.Id]
  ): F[Option[ExecutionVisits]] =
    Sync[F].delay(???)

  def querySequence(@unused obsId: Observation.Id, @unused includeItc: Boolean): F[SequenceData] =
    Sync[F].delay(???)

  def insertSmartGcal(
    @unused obsId:         Observation.Id,
    @unused smartGcalType: SmartGcalType
  ): F[Unit] =
    Sync[F].delay(???)

  def clearInsertedSteps(@unused obsId: Observation.Id, @unused stepIds: Set[Step.Id]): F[Unit] =
    Sync[F].delay(???)

object ODBQueryApi:
  // Default value noop implementations with warning
  val ctx: Context[ODBQueryApi[IO]] = React.createContext(new ODBQueryApi[IO] {})
