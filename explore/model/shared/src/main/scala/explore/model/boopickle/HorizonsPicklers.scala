// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic.*
import lucuma.core.model.AirMass
import lucuma.core.model.EphemerisKey
import lucuma.core.model.Extinction
import lucuma.horizons.HorizonsClient.ElementsPerDay
import lucuma.horizons.HorizonsEphemeris
import lucuma.horizons.HorizonsEphemerisEntry

trait HorizonsPicklers extends CommonPicklers:
  def int2ElementsPerDay(i: Int): ElementsPerDay = i match
    case 1  => 1
    case 2  => 2
    case 3  => 3
    case 4  => 4
    case 6  => 6
    case 8  => 8
    case 12 => 12
    case 24 => 24
    case _  => sys.error("Cannot unpickle ElementsPerDay")

  given Pickler[ElementsPerDay] =
    transformPickler[ElementsPerDay, Int](int2ElementsPerDay)(identity)

  given Pickler[EphemerisKey.Comet]       = generatePickler
  given Pickler[EphemerisKey.AsteroidNew] = generatePickler
  given Pickler[EphemerisKey.AsteroidOld] = generatePickler
  given Pickler[EphemerisKey.MajorBody]   = generatePickler
  given Pickler[EphemerisKey.Horizons]    =
    compositePickler[EphemerisKey.Horizons]
      .addConcreteType[EphemerisKey.Comet]
      .addConcreteType[EphemerisKey.AsteroidNew]
      .addConcreteType[EphemerisKey.AsteroidOld]
      .addConcreteType[EphemerisKey.MajorBody]

  given Pickler[AirMass]    = picklerNewType(AirMass)
  given Pickler[Extinction] = picklerNewType(Extinction)

  given Pickler[HorizonsEphemerisEntry] = generatePickler
  given Pickler[HorizonsEphemeris]      = generatePickler

object HorizonsPicklers extends HorizonsPicklers
