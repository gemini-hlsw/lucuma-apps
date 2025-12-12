// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import lucuma.ags.AgsAnalysis
import lucuma.core.math.Angle

case class CandidateAnalysis(
  analysis: AgsAnalysis,
  usableAt: Option[Angle]
) derives Eq:
  def reachableAtOtherPAs: Boolean = usableAt.isDefined

object CandidateAnalysis:
  def fromAnalysis(analysis: AgsAnalysis): CandidateAnalysis =
    CandidateAnalysis(analysis, usableAt = None)
