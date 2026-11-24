// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.ui.input.InputCodecTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

/**
 * The codecs of the editable sequence table's cells. Offsets are checked up to 99999.99", which is
 * all their mask accepts (see `InputCodecsSuite` in Explore for the same limit in the offsets
 * editor), and wavelengths over the instruments' range.
 */
final class SequenceCodecsSuite extends DisciplineSuite:
  private val offsetAngles: Arbitrary[Angle] = Arbitrary(
    Gen.choose(-99999990000L, 99999990000L).map(Angle.fromMicroarcseconds)
  )

  private given Arbitrary[Wavelength] = Arbitrary(
    Gen.choose(300000, 30000000).map(Wavelength.unsafeFromIntPicometers)
  )

  checkAll(
    "OffsetArcsecondsCodec",
    InputCodecTests(SequenceColumns.OffsetArcsecondsCodec)(using offsetAngles)
  )
  checkAll("SequenceWavelengthFormat.Nanometers",
           InputCodecTests(SequenceWavelengthFormat.Nanometers.codec)
  )
