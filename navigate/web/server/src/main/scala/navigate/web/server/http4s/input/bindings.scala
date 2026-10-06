// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s.input

import cats.syntax.all.*
import lucuma.core.enums.M1Source
import lucuma.core.enums.TipTiltSource
import lucuma.core.math.Angle
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.AngleInput
import lucuma.schemas.model.navigate.LightSinkVariant
import lucuma.schemas.model.navigate.LightSource
import navigate.model.RotatorTrackingMode
import navigate.model.enums.AcFilter
import navigate.model.enums.AcLens
import navigate.model.enums.AcNdFilter
import navigate.model.enums.AcquisitionAdjustmentCommand
import navigate.model.enums.CentralBafflePosition
import navigate.model.enums.DeployableBafflePosition
import navigate.model.enums.DomeMode
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter
import navigate.model.enums.QlMode
import navigate.model.enums.VirtualTelescope

val AcFilterBinding: Matcher[AcFilter]                                         = enumeratedBinding
val AcLensBinding: Matcher[AcLens]                                             = enumeratedBinding
val AcNdFilterBinding: Matcher[AcNdFilter]                                     = enumeratedBinding
val AcquisitionAdjustmentCommandBinding: Matcher[AcquisitionAdjustmentCommand] = enumeratedBinding
val CentralBafflePositionBinding: Matcher[CentralBafflePosition]               = enumeratedBinding
val DeployableBafflePositionBinding: Matcher[DeployableBafflePosition]         = enumeratedBinding
val DomeModeBinding: Matcher[DomeMode]                                         = enumeratedBinding
val LightSinkVariantBinding: Matcher[LightSinkVariant]                         = enumeratedBinding
val LightSourceBinding: Matcher[LightSource]                                   = enumeratedBinding
val M1SourceBinding: Matcher[M1Source]                                         = enumeratedBinding
val PwfsFieldStopBinding: Matcher[PwfsFieldStop]                               = enumeratedBinding
val PwfsFilterBinding: Matcher[PwfsFilter]                                     = enumeratedBinding
val QlModeBinding: Matcher[QlMode]                                             = enumeratedBinding
val RotatorTrackingModeBinding: Matcher[RotatorTrackingMode]                   = enumeratedBinding
val TipTiltSourceBinding: Matcher[TipTiltSource]                               = enumeratedBinding
val VirtualTelescopeBinding: Matcher[VirtualTelescope]                         = enumeratedBinding

/** An input object with exactly two `AngleInput` fields, `a` and `b`, in that order. */
def anglePairBinding[A](a: String, b: String)(f: (Angle, Angle) => A): Matcher[A] =
  ObjectFieldsBinding.rmap:
    case List(AngleInput.Binding(`a`, ra), AngleInput.Binding(`b`, rb)) => (ra, rb).parMapN(f)
