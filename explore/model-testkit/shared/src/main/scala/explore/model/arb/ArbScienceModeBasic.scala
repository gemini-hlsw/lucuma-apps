// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import explore.model.ScienceModeBasic
import lucuma.core.util.arb.ArbGid.*
import lucuma.core.util.arb.ArbEnumerated.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosSouthFpu

trait ArbScienceModeBasic {

  given Arbitrary[ScienceModeBasic.GmosNorthLongSlit] =
    Arbitrary[ScienceModeBasic.GmosNorthLongSlit](
      for {
        grating <- arbitrary[GmosNorthGrating]
        filter  <- arbitrary[Option[GmosNorthFilter]]
        fpu     <- arbitrary[GmosNorthFpu]
      } yield ScienceModeBasic.GmosNorthLongSlit(grating, filter, fpu)
    )

  given Arbitrary[ScienceModeBasic.GmosSouthLongSlit] =
    Arbitrary[ScienceModeBasic.GmosSouthLongSlit](
      for {
        grating <- arbitrary[GmosSouthGrating]
        filter  <- arbitrary[Option[GmosSouthFilter]]
        fpu     <- arbitrary[GmosSouthFpu]
      } yield ScienceModeBasic.GmosSouthLongSlit(grating, filter, fpu)
    )

  given Arbitrary[ScienceModeBasic] = Arbitrary[ScienceModeBasic](
    Gen.oneOf(
      arbitrary[ScienceModeBasic.GmosNorthLongSlit],
      arbitrary[ScienceModeBasic.GmosSouthLongSlit]
    )
  )

  given Cogen[ScienceModeBasic.GmosNorthLongSlit] =
    Cogen[(GmosNorthGrating, Option[GmosNorthFilter], GmosNorthFpu)]
      .contramap(o => (o.grating, o.filter, o.fpu))

  given Cogen[ScienceModeBasic.GmosSouthLongSlit] =
    Cogen[(GmosSouthGrating, Option[GmosSouthFilter], GmosSouthFpu)]
      .contramap(o => (o.grating, o.filter, o.fpu))

  given Cogen[ScienceModeBasic] =
    Cogen[Either[ScienceModeBasic.GmosNorthLongSlit, ScienceModeBasic.GmosSouthLongSlit]]
      .contramap {
        case n @ ScienceModeBasic.GmosNorthLongSlit(_, _, _) => n.asLeft
        case s @ ScienceModeBasic.GmosSouthLongSlit(_, _, _) => s.asRight
      }

}

object ArbScienceModeBasic extends ArbScienceModeBasic
