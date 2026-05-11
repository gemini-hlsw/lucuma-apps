// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic.*
import lucuma.core.model.AirMass
import lucuma.core.model.Ephemeris
import lucuma.core.model.Extinction
import lucuma.horizons.HorizonsClient.ElementsPerDay

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

  given Pickler[Ephemeris.Key.Comet]       = generatePickler
  given Pickler[Ephemeris.Key.AsteroidNew] = generatePickler
  given Pickler[Ephemeris.Key.AsteroidOld] = generatePickler
  given Pickler[Ephemeris.Key.MajorBody]   = generatePickler
  given Pickler[Ephemeris.Key.Horizons]    =
    compositePickler[Ephemeris.Key.Horizons]
      .addConcreteType[Ephemeris.Key.Comet]
      .addConcreteType[Ephemeris.Key.AsteroidNew]
      .addConcreteType[Ephemeris.Key.AsteroidOld]
      .addConcreteType[Ephemeris.Key.MajorBody]

  given Pickler[AirMass]    = picklerNewType(AirMass)
  given Pickler[Extinction] = picklerNewType(Extinction)

object HorizonsPicklers extends HorizonsPicklers
