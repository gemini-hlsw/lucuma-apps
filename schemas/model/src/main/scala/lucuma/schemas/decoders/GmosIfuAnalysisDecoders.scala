// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.decoders

import io.circe.Decoder
import io.circe.DecodingFailure
import lucuma.core.math.Angle
import lucuma.core.model.GmosIfuAnalysis
import lucuma.odb.json.angle.decoder.given

trait GmosIfuAnalysisDecoders:

  /**
   * `GmosIfuAnalysis` is a `@oneOf`: exactly one of the two fields is set, and which one decides
   * how the ITC samples the field.
   */
  given Decoder[GmosIfuAnalysis] = Decoder.instance: c =>
    for
      sumRadius    <- c.downField("sumRadius").as[Option[Angle]]
      singleOffset <- c.downField("singleOffset").as[Option[Angle]]
      analysis     <- (sumRadius, singleOffset) match
                        case (Some(radius), None) => Right(GmosIfuAnalysis.Sum(radius))
                        case (None, Some(offset)) => Right(GmosIfuAnalysis.Single(offset))
                        case _                    =>
                          Left(
                            DecodingFailure(
                              "GmosIfuAnalysis must have exactly one of sumRadius or singleOffset",
                              c.history
                            )
                          )
    yield analysis
