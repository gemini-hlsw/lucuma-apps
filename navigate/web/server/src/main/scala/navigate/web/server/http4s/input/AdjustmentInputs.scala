// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s.input

import cats.syntax.all.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.AngleInput
import lucuma.odb.graphql.input.OffsetInput
import navigate.model.AcquisitionAdjustment
import navigate.model.FocalPlaneOffset
import navigate.model.FocalPlaneOffset.DeltaX
import navigate.model.FocalPlaneOffset.DeltaY
import navigate.model.HandsetAdjustment

object HandsetAdjustmentInput:

  private val HorizontalAdjustmentBinding: Matcher[HandsetAdjustment] =
    anglePairBinding("azimuth", "elevation")(HandsetAdjustment.HorizontalAdjustment.apply)

  private val FocalPlaneAdjustmentBinding: Matcher[HandsetAdjustment] =
    anglePairBinding("deltaX", "deltaY"): (dx, dy) =>
      HandsetAdjustment.FocalPlaneAdjustment(FocalPlaneOffset(DeltaX(dx), DeltaY(dy)))

  private val InstrumentAdjustmentBinding: Matcher[HandsetAdjustment] =
    OffsetInput.Binding.map(HandsetAdjustment.InstrumentAdjustment.apply)

  private val EquatorialAdjustmentBinding: Matcher[HandsetAdjustment] =
    anglePairBinding("deltaRA", "deltaDec")(HandsetAdjustment.EquatorialAdjustment.apply)

  private val ProbeFrameAdjustmentBinding: Matcher[HandsetAdjustment] =
    ObjectFieldsBinding.rmap:
      case List(
            GuideProbeBinding("probeFrame", rProbeFrame),
            AngleInput.Binding("deltaU", rDeltaU),
            AngleInput.Binding("deltaV", rDeltaV),
            AngleInput.Binding.Option("alignAngle", rAlignAngle)
          ) =>
        (rProbeFrame, rDeltaU, rDeltaV, rAlignAngle).parMapN(
          HandsetAdjustment.ProbeFrameAdjustment.apply
        )

  val Binding: Matcher[HandsetAdjustment] =
    OneOfBinding(
      "horizontalAdjustment" -> HorizontalAdjustmentBinding,
      "focalPlaneAdjustment" -> FocalPlaneAdjustmentBinding,
      "instrumentAdjustment" -> InstrumentAdjustmentBinding,
      "equatorialAdjustment" -> EquatorialAdjustmentBinding,
      "probeFrameAdjustment" -> ProbeFrameAdjustmentBinding
    )

object AcquisitionAdjustmentInput:

  val Binding: Matcher[AcquisitionAdjustment] =
    ObjectFieldsBinding.rmap:
      case List(
            OffsetInput.Binding("offset", rOffset),
            AngleInput.Binding.Option("ipa", rIpa),
            AngleInput.Binding.Option("iaa", rIaa),
            AcquisitionAdjustmentCommandBinding("command", rCommand)
          ) =>
        (rOffset, rIpa, rIaa, rCommand).parMapN(AcquisitionAdjustment.apply)
