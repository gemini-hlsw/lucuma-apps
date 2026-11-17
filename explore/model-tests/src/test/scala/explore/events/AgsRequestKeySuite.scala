// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import cats.data.NonEmptyList
import cats.syntax.all.*
import explore.boopickle.roundTrip
import explore.events.AgsMessage.given
import explore.model.boopickle.Boopickle.asKeyBytes
import explore.model.boopickle.CatalogPicklers
import lucuma.ags.AgsParams
import lucuma.ags.GuideStarCandidate
import lucuma.ags.GuideStarId
import lucuma.ags.arb.ArbGuideStarCandidate.given
import lucuma.core.enums.AltairMode
import lucuma.core.enums.Band
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbCoordinates.given
import lucuma.core.math.arb.ArbWavelength.given
import lucuma.core.model.ConstraintSet
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbConstraintSet.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.schemas.model.AGSWavelength
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import java.time.Instant
import scala.collection.immutable.SortedMap

// Regression test for the duplicated AGS worker cache-key bug fix.
// Don't ever revert to identity-sensitive key derivation.
class AgsRequestKeySuite extends ScalaCheckSuite, CatalogPicklers:

  def agsRequest(
    id:         Target.Id,
    cs:         ConstraintSet,
    w:          Wavelength,
    base:       Coordinates,
    candidates: List[GuideStarCandidate],
    params:     AgsParams
  ): AgsMessage.AgsRequest =
    AgsMessage.AgsRequest(
      id,
      Instant.EPOCH,
      cs,
      AGSWavelength(w),
      base,
      List(base),
      none,
      NonEmptyList.one(Angle.Angle0),
      none,
      none,
      params,
      candidates
    )

  property("asKeyBytes produces stable cache keys"):
    forAll:
      (
        id:   Target.Id,
        gs:   GuideStarCandidate,
        cs:   ConstraintSet,
        w:    Wavelength,
        base: Coordinates
      ) =>
        // pickle and unpickle a guidestar candidate
        val gsCopy            = roundTrip(gs)
        // The mode selection is not very important for this test.
        val params: AgsParams = AgsParams.GmosImaging(PortDisposition.Side)
        val shared            = agsRequest(id, cs, w, base, List(gs, gs), params)
        val distinct          = agsRequest(id, cs, w, base, List(gs, gsCopy), params)
        assertEquals(shared, distinct) // value-equal requests
        // asBytes does not always produce the same bytes for the same object
        assert(asKeyBytes(shared).sameElements(asKeyBytes(distinct)))

  property("Altair params survive pickling and key the cache by mode"):
    forAll:
      (
        id:   Target.Id,
        gs:   GuideStarCandidate,
        cs:   ConstraintSet,
        w:    Wavelength,
        base: Coordinates,
        mode: AltairMode
      ) =>
        val gnirs: AgsParams.GnirsLongSlit   =
          AgsParams.GnirsLongSlit(GnirsFpuSlit.LongSlit_0_30,
                                  GnirsCamera.ShortBlue,
                                  GnirsPrism.Mirror
          )
        val altair: AgsMessage.AgsRequest    =
          agsRequest(id, cs, w, base, List(gs), gnirs.withAltair(mode))
        val unpickled: AgsMessage.AgsRequest = roundTrip(altair)
        assertEquals(unpickled.params.altair, mode.some)
        assertEquals(unpickled.params.probe, mode.guideProbe)
        assert(asKeyBytes(unpickled).sameElements(asKeyBytes(altair)))
        val plain: AgsMessage.AgsRequest     = agsRequest(id, cs, w, base, List(gs), gnirs)
        assert(!asKeyBytes(plain).sameElements(asKeyBytes(altair)))

  property("an observation target candidate that moves or changes brightness changes the key"):
    forAll:
      (
        id:       Target.Id,
        gs:       GuideStarCandidate,
        cs:       ConstraintSet,
        w:        Wavelength,
        base:     Coordinates,
        moved:    Coordinates,
        targetId: Target.Id
      ) =>
        val params: AgsParams                      = AgsParams.GmosImaging(PortDisposition.Side)
        val science: GuideStarCandidate            =
          GuideStarCandidate(GuideStarId.ObservationTarget(targetId.some),
                             SiderealTracking.const(base),
                             SortedMap(Band.R -> BrightnessValue.unsafeFrom(12.0))
          )
        val brighter: GuideStarCandidate           =
          GuideStarCandidate.brightnesses.replace(
            SortedMap(Band.R -> BrightnessValue.unsafeFrom(11.0))
          )(
            science
          )
        val elsewhere: GuideStarCandidate          =
          GuideStarCandidate.tracking.replace(SiderealTracking.const(moved))(science)
        val key: GuideStarCandidate => Array[Byte] =
          candidate => asKeyBytes(agsRequest(id, cs, w, base, List(gs, candidate), params))
        assert(!key(science).sameElements(key(brighter)))
        assert(moved === base || !key(science).sameElements(key(elsewhere)))
