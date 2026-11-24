// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.syntax.all.*
import lucuma.core.enums.WavelengthOrder
import lucuma.core.model.sequence.gmos.imaging.DefaultPreImagingOffsets
import lucuma.refined.*
import lucuma.schemas.model.arb.ArbImagingVariant.genPreImaging
import lucuma.schemas.model.arb.ArbImagingVariant.given
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

class ImagingVariantSuite extends ScalaCheckSuite:
  private def offsets(v: ImagingVariant.PreImaging) =
    (v.offset1, v.offset2, v.offset3, v.offset4)

  property("turning pre-imaging on starts from the core offsets"):
    forAll: (v: ImagingVariant) =>
      v.withPreImaging(true) match
        case p: ImagingVariant.PreImaging =>
          if v.isPreImaging then assertEquals(p, v)
          else assertEquals(offsets(p), DefaultPreImagingOffsets)
        case other                        => fail(s"Expected pre-imaging, got $other")

  property("turning pre-imaging off lands on grouped with no offsets"):
    forAll(genPreImaging): v =>
      assertEquals(
        v.withPreImaging(false),
        ImagingVariant.Grouped(
          WavelengthOrder.Increasing,
          none,
          0.refined,
          none,
          v.exposuresPerOffset
        )
      )

  property("turning pre-imaging off leaves grouped and interleaved alone"):
    forAll: (v: ImagingVariant) =>
      if !v.isPreImaging then assertEquals(v.withPreImaging(false), v)

  property("exposures per offset survives both ways"):
    forAll: (v: ImagingVariant) =>
      assertEquals(v.withPreImaging(true).exposuresPerOffset, v.exposuresPerOffset)
      assertEquals(v.withPreImaging(false).exposuresPerOffset, v.exposuresPerOffset)

  property("isPreImaging follows the flag"):
    forAll: (v: ImagingVariant) =>
      assert(v.withPreImaging(true).isPreImaging)
      assert(!v.withPreImaging(false).isPreImaging)
