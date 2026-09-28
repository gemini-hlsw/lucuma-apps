// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GuideProbe
import lucuma.odb.data.AltairConfiguration
import monocle.Focus
import monocle.Lens

// The guide probe override and the Altair configuration. Every guider choice sets both, and the
// ODB validates the probe against the Altair mode, so they are edited and sent as one unit.
case class GuidingConfiguration(
  explicitGuideProbe: Option[GuideProbe],
  altair:             Option[AltairConfiguration]
) derives Eq

object GuidingConfiguration:
  val explicitGuideProbe: Lens[GuidingConfiguration, Option[GuideProbe]] =
    Focus[GuidingConfiguration](_.explicitGuideProbe)

  val altair: Lens[GuidingConfiguration, Option[AltairConfiguration]] =
    Focus[GuidingConfiguration](_.altair)

  val Empty: GuidingConfiguration = GuidingConfiguration(None, None)
