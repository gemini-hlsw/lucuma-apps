// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.odb.json.gnirs.given
import lucuma.schemas.ObservationDB

@GraphQL
object GnirsDynamicConfigSubquery
    extends GraphQLSubquery.Typed[ObservationDB, GnirsDynamicConfig]("GnirsDynamic"):
  override val subquery: String = s"""
    {
      exposure $TimeSpanSubquery
      coadds
      centralWavelength $WavelengthSubquery
      filter
      decker
      fpuSlit
      fpuOther
      acquisitionMirrorOut {
        prism
        grating
        wavelength $WavelengthSubquery
      }
      camera
      focusMotorSteps
      readMode
    }
  """
