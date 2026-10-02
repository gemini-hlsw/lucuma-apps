// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.tcs

import algebra.instances.all.*
import coulomb.*
import coulomb.conversion.implicits.given
import coulomb.syntax.*
import coulomb.units.accepted.ArcSecond
import coulomb.units.accepted.Degree
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait TcsArbitraries {
  private def rangedAngleGen(
    minVal: Quantity[Double, ArcSecond],
    maxVal: Quantity[Double, ArcSecond]
  ) =
    Gen.choose(minVal.value, maxVal.value)

  private val offsetLimit: Quantity[Double, ArcSecond] = 120.0.withUnit[ArcSecond]

  given Arbitrary[TcsController.OffsetP]          = Arbitrary(
    rangedAngleGen(-offsetLimit, offsetLimit).map(u =>
      TcsController.OffsetP.apply(u.withUnit[Degree].toValue[Double])
    )
  )
  given Cogen[TcsController.OffsetP]              =
    Cogen[Double].contramap(_.value.value)
  given Arbitrary[TcsController.OffsetQ]          = Arbitrary(
    rangedAngleGen(-offsetLimit, offsetLimit).map(u =>
      TcsController.OffsetQ.apply(u.withUnit[Degree])
    )
  )
  given Cogen[TcsController.OffsetQ]              =
    Cogen[Double].contramap(_.value.value)
  given Arbitrary[TcsController.InstrumentOffset] = Arbitrary {
    for {
      p <- arbitrary[TcsController.OffsetP]
      q <- arbitrary[TcsController.OffsetQ]
    } yield TcsController.InstrumentOffset(p, q)
  }
  given Cogen[TcsController.InstrumentOffset]     =
    Cogen[(TcsController.OffsetP, TcsController.OffsetQ)].contramap(x => (x.p, x.q))

  given Arbitrary[Quantity[Double, ArcSecond]] = Arbitrary(
    rangedAngleGen((-90.0).withUnit[Degree], 270.0.withUnit[Degree]).map(_.withUnit[ArcSecond])
  )
  given Cogen[Quantity[Double, ArcSecond]]     = Cogen[Double].contramap(_.value)

  given Arbitrary[CRFollow] = Arbitrary {
    Gen.oneOf(CRFollow.On, CRFollow.Off)
  }
  given Cogen[CRFollow]     =
    Cogen[String].contramap(_.productPrefix)

}
