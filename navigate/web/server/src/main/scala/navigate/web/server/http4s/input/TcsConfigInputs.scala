// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s.input

import cats.syntax.all.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.AngleInput
import lucuma.odb.graphql.input.WavelengthInput
import lucuma.odb.graphql.input.atMostOne
import navigate.model.AutoparkAowfs
import navigate.model.AutoparkGems
import navigate.model.AutoparkOiwfs
import navigate.model.AutoparkPwfs1
import navigate.model.AutoparkPwfs2
import navigate.model.BafflesConfig
import navigate.model.GuiderConfig
import navigate.model.InstrumentSpecifics
import navigate.model.Origin
import navigate.model.ResetPointing
import navigate.model.RotatorTrackConfig
import navigate.model.ShortcircuitMountFilter
import navigate.model.ShortcircuitTargetFilter
import navigate.model.SlewOptions
import navigate.model.StopGuide
import navigate.model.SwapConfig
import navigate.model.TcsConfig
import navigate.model.TrackingConfig
import navigate.model.ZeroChopThrow
import navigate.model.ZeroGuideOffset
import navigate.model.ZeroInstrumentOffset
import navigate.model.ZeroMountDiffTrack
import navigate.model.ZeroMountOffset
import navigate.model.ZeroSourceDiffTrack
import navigate.model.ZeroSourceOffset

object PointOriginInput:

  val Binding: Matcher[Origin] =
    anglePairBinding("x", "y")(Origin.apply)

object InstrumentSpecificsInput:

  val Binding: Matcher[InstrumentSpecifics] =
    ObjectFieldsBinding.rmap:
      case List(
            AngleInput.Binding("iaa", rIaa),
            DistanceInput.Binding("focusOffset", rFocusOffset),
            StringBinding("agName", rAgName),
            PointOriginInput.Binding("origin", rOrigin)
          ) =>
        (rIaa, rFocusOffset, rAgName, rOrigin).parMapN(InstrumentSpecifics.apply)

object ProbeTrackingInput:

  val Binding: Matcher[TrackingConfig] =
    ObjectFieldsBinding.rmap:
      case List(
            BooleanBinding("nodAchopA", rNodAChopA),
            BooleanBinding("nodAchopB", rNodAChopB),
            BooleanBinding("nodBchopA", rNodBChopA),
            BooleanBinding("nodBchopB", rNodBChopB)
          ) =>
        (rNodAChopA, rNodAChopB, rNodBChopA, rNodBChopB).parMapN(TrackingConfig.apply)

object GuiderConfigInput:

  val Binding: Matcher[GuiderConfig] =
    ObjectFieldsBinding.rmap:
      case List(
            GuideTargetPropertiesInput.Binding("target", rTarget),
            ProbeTrackingInput.Binding("tracking", rTracking)
          ) =>
        (rTarget, rTracking).parMapN(GuiderConfig.apply)

object RotatorTrackingInput:

  val Binding: Matcher[RotatorTrackConfig] =
    ObjectFieldsBinding.rmap:
      case List(
            AngleInput.Binding("ipa", rIpa),
            RotatorTrackingModeBinding("mode", rMode)
          ) =>
        (rIpa, rMode).parMapN(RotatorTrackConfig.apply)

object BaffleConfigInput:

  val AutoBinding: Matcher[BafflesConfig] =
    ObjectFieldsBinding.rmap:
      case List(
            WavelengthInput.Binding("visibleLimit", rVisibleLimit),
            WavelengthInput.Binding("nearirLimit", rNearIrLimit)
          ) =>
        (rVisibleLimit, rNearIrLimit).parMapN(BafflesConfig.AutoConfig.apply)

  val ManualBinding: Matcher[BafflesConfig] =
    ObjectFieldsBinding.rmap:
      case List(
            CentralBafflePositionBinding("centralBaffle", rCentral),
            DeployableBafflePositionBinding("deployableBaffle", rDeployable)
          ) =>
        (rCentral, rDeployable).parMapN(BafflesConfig.ManualConfig.apply)

  /** An empty baffle configuration means that the baffles are not configured. */
  val Binding: Matcher[Option[BafflesConfig]] =
    ObjectFieldsBinding.rmap:
      case List(
            AutoBinding.Option("autoConfig", rAuto),
            ManualBinding.Option("manualConfig", rManual)
          ) =>
        (rAuto, rManual).parTupled.flatMap: (auto, manual) =>
          atMostOne(auto -> "autoConfig", manual -> "manualConfig")

object TcsConfigInput:

  val Binding: Matcher[TcsConfig] =
    ObjectFieldsBinding.rmap:
      case List(
            TargetPropertiesInput.Binding("sourceATarget", rTarget),
            InstrumentSpecificsInput.Binding("instParams", rInstParams),
            GuiderConfigInput.Binding.Option("pwfs1", rPwfs1),
            GuiderConfigInput.Binding.Option("pwfs2", rPwfs2),
            GuiderConfigInput.Binding.Option("oiwfs", rOiwfs),
            RotatorTrackingInput.Binding("rotator", rRotator),
            InstrumentBinding("instrument", rInstrument),
            LightSinkVariantBinding.Option("lightSinkVariant", rLightSinkVariant),
            BaffleConfigInput.Binding.Option("baffles", rBaffles)
          ) =>
        (rTarget,
         rInstParams,
         rPwfs1,
         rPwfs2,
         rOiwfs,
         rRotator,
         LightPathInput.lightSink(rInstrument, rLightSinkVariant),
         rBaffles.map(_.flatten)
        )
          .parMapN(TcsConfig.apply)

object SwapConfigInput:

  val Binding: Matcher[SwapConfig] =
    ObjectFieldsBinding.rmap:
      case List(
            TargetPropertiesInput.Binding("guideTarget", rTarget),
            InstrumentSpecificsInput.Binding("acParams", rAcParams),
            RotatorTrackingInput.Binding("rotator", rRotator)
          ) =>
        (rTarget, rAcParams, rRotator).parMapN(SwapConfig.apply)

object SlewOptionsInput:

  val Binding: Matcher[SlewOptions] =
    ObjectFieldsBinding.rmap:
      case List(
            BooleanBinding("zeroChopThrow", rZeroChopThrow),
            BooleanBinding("zeroSourceOffset", rZeroSourceOffset),
            BooleanBinding("zeroSourceDiffTrack", rZeroSourceDiffTrack),
            BooleanBinding("zeroMountOffset", rZeroMountOffset),
            BooleanBinding("zeroMountDiffTrack", rZeroMountDiffTrack),
            BooleanBinding("shortcircuitTargetFilter", rShortcircuitTargetFilter),
            BooleanBinding("shortcircuitMountFilter", rShortcircuitMountFilter),
            BooleanBinding("resetPointing", rResetPointing),
            BooleanBinding("stopGuide", rStopGuide),
            BooleanBinding("zeroGuideOffset", rZeroGuideOffset),
            BooleanBinding("zeroInstrumentOffset", rZeroInstrumentOffset),
            BooleanBinding("autoparkPwfs1", rAutoparkPwfs1),
            BooleanBinding("autoparkPwfs2", rAutoparkPwfs2),
            BooleanBinding("autoparkOiwfs", rAutoparkOiwfs),
            BooleanBinding("autoparkGems", rAutoparkGems),
            BooleanBinding("autoparkAowfs", rAutoparkAowfs)
          ) =>
        (
          rZeroChopThrow.map(ZeroChopThrow.apply),
          rZeroSourceOffset.map(ZeroSourceOffset.apply),
          rZeroSourceDiffTrack.map(ZeroSourceDiffTrack.apply),
          rZeroMountOffset.map(ZeroMountOffset.apply),
          rZeroMountDiffTrack.map(ZeroMountDiffTrack.apply),
          rShortcircuitTargetFilter.map(ShortcircuitTargetFilter.apply),
          rShortcircuitMountFilter.map(ShortcircuitMountFilter.apply),
          rResetPointing.map(ResetPointing.apply),
          rStopGuide.map(StopGuide.apply),
          rZeroGuideOffset.map(ZeroGuideOffset.apply),
          rZeroInstrumentOffset.map(ZeroInstrumentOffset.apply),
          rAutoparkPwfs1.map(AutoparkPwfs1.apply),
          rAutoparkPwfs2.map(AutoparkPwfs2.apply),
          rAutoparkOiwfs.map(AutoparkOiwfs.apply),
          rAutoparkGems.map(AutoparkGems.apply),
          rAutoparkAowfs.map(AutoparkAowfs.apply)
        ).parMapN(SlewOptions.apply)
