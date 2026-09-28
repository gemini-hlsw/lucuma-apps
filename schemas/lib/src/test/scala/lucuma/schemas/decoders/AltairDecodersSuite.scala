// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.decoders

import cats.syntax.all.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.AltairNdFilter
import lucuma.core.enums.CassRotator
import lucuma.core.enums.FieldLens
import lucuma.odb.data.AltairConfiguration

class AltairDecodersSuite extends InputStreamSuite:

  test("NGS with an explicit field lens"):
    assertParsedStreamEquals(
      "/altair1.json",
      AltairConfiguration(AltairMode.Ngs, FieldLens.Out.some, CassRotator.Fixed, AltairNdFilter.In)
    )

  test("LGS+P1 with an automatic field lens"):
    assertParsedStreamEquals(
      "/altair2.json",
      AltairConfiguration(AltairMode.LgsP1, none, CassRotator.Following, AltairNdFilter.Out)
    )
