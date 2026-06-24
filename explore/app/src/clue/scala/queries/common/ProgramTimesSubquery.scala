// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import explore.model.ProgramTimes
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("Program")
object ProgramTimesSubquery extends GraphQLSubquery.Typed[ObservationDB, ProgramTimes] {
  override val subquery: String = s"""
    {
      timeEstimateRange $CalculatedProgramTimeRangeSubquery
      timeEstimateBanded $CalculatedBandedProgramTimeSubquery
      timeCharge $BandedProgramTimeSubquery
    }
  """
}
