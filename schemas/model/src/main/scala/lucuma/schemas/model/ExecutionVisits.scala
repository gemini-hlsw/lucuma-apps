// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.eq.*
import lucuma.core.enums.Instrument
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

enum ExecutionVisits(val instrument: Instrument) derives Eq:

  private def removeDuplicateVisitOverlap[D, V <: Visit[D]](
    left:  NonEmptyList[V],
    right: NonEmptyList[V]
  ): NonEmptyList[V] =
    NonEmptyList
      .fromList:
        left.toList.takeWhile: v =>
          right.head.id =!= v.id
      .fold(right)(_ ++ right.toList)

  def extendWith(other: ExecutionVisits): ExecutionVisits =
    (this, other) match
      case (GmosNorth(leftVisits), GmosNorth(rightVisits))   =>
        GmosNorth(removeDuplicateVisitOverlap(leftVisits, rightVisits))
      case (GmosSouth(leftVisits), GmosSouth(rightVisits))   =>
        GmosSouth(removeDuplicateVisitOverlap(leftVisits, rightVisits))
      case (Flamingos2(leftVisits), Flamingos2(rightVisits)) =>
        Flamingos2(removeDuplicateVisitOverlap(leftVisits, rightVisits))
      case (Igrins2(leftVisits), Igrins2(rightVisits))       =>
        Igrins2(removeDuplicateVisitOverlap(leftVisits, rightVisits))
      case (left, right)                                     =>
        throw new Exception:
          s"Attempted to join ExecutionVisits for different instruments: ${left.instrument} and ${right.instrument}"

  case GmosNorth(visits: NonEmptyList[Visit.GmosNorth])
      extends ExecutionVisits(Instrument.GmosNorth)

  case GmosSouth(visits: NonEmptyList[Visit.GmosSouth])
      extends ExecutionVisits(Instrument.GmosSouth)

  case Flamingos2(visits: NonEmptyList[Visit.Flamingos2])
      extends ExecutionVisits(Instrument.Flamingos2)

  case Igrins2(visits: NonEmptyList[Visit.Igrins2]) extends ExecutionVisits(Instrument.Igrins2)

object ExecutionVisits:
  val gmosNorth: Prism[ExecutionVisits, ExecutionVisits.GmosNorth] =
    GenPrism[ExecutionVisits, ExecutionVisits.GmosNorth]

  val gmosSouth: Prism[ExecutionVisits, ExecutionVisits.GmosSouth] =
    GenPrism[ExecutionVisits, ExecutionVisits.GmosSouth]

  val flamingos2: Prism[ExecutionVisits, ExecutionVisits.Flamingos2] =
    GenPrism[ExecutionVisits, ExecutionVisits.Flamingos2]

  val igrins2: Prism[ExecutionVisits, ExecutionVisits.Igrins2] =
    GenPrism[ExecutionVisits, ExecutionVisits.Igrins2]

  object GmosNorth:
    val visits: Lens[GmosNorth, NonEmptyList[Visit.GmosNorth]] =
      Focus[GmosNorth](_.visits)

  object GmosSouth:
    val visits: Lens[GmosSouth, NonEmptyList[Visit.GmosSouth]] =
      Focus[GmosSouth](_.visits)

  object Flamingos2:
    val visits: Lens[Flamingos2, NonEmptyList[Visit.Flamingos2]] =
      Focus[Flamingos2](_.visits)

  object Igrins2:
    val visits: Lens[Igrins2, NonEmptyList[Visit.Igrins2]] =
      Focus[Igrins2](_.visits)
