// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import lucuma.core.math.Angle.Angle360µas

import LinearAngle.Angle270µas
import LinearAngle.MinusAngle180µas
import LinearAngle.MinusAngle270µas

object AzimuthAngle extends LinearAngle[MinusAngle180µas, Angle360µas]
type AzimuthAngle = AzimuthAngle.Type

object RotatorAngle extends LinearAngle[MinusAngle270µas, Angle270µas]
type RotatorAngle = RotatorAngle.Type
