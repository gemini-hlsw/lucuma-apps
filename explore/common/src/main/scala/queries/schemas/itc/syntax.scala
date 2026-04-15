// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas.itc

import cats.Hash
import cats.data.EitherNec
import cats.data.NonEmptyList
import cats.syntax.all.*
import explore.model.Constants
import explore.model.TargetList
import explore.model.itc.ItcQueryProblem
import explore.model.itc.ItcTarget
import explore.model.itc.ItcTargetProblem
import explore.modes.ItcInstrumentConfig
import explore.optics.ModelOptics.*
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.GmosRoi
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.model.*
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.itc.ItcGhostDetector
import lucuma.itc.client.GmosFpu
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.TargetInput

import scala.collection.immutable.SortedSet

trait syntax:

  extension (row: ItcInstrumentConfig)
    // GHOST NOTE: This will need to return anEitherNec[ItcQueryProblem, ItcAsterismGraphResults]]
    // and take the list of targets or their number as a parameter to validate the GHOST mode.
    // (Standard resolution can have one or 2 targets, high resolution can only have one.)
    // Actually, we may need to validate the targets earlier, because the GHOST ItcInstrumentConfig
    // will need to assign targets to CCDs.
    // It will also need to validate that the ETM is Time and Count.
    def toItcClientMode(targetCount: Int): EitherNec[ItcQueryProblem, InstrumentMode] =
      def validateGhostMode(
        ghost:       ItcInstrumentConfig.GhostIfu,
        targetCount: Int
      ): EitherNec[ItcQueryProblem, InstrumentMode] =
        if (ghost.resolutionMode == GhostResolutionMode.Standard && targetCount > 2)
          ItcQueryProblem
            .GenericError(
              s"GHOST standard resolution mode supports up to 2 targets, but $targetCount were provided."
            )
            .leftNec
        else if (ghost.resolutionMode == GhostResolutionMode.High && targetCount > 1)
          ItcQueryProblem
            .GenericError(
              s"GHOST high resolution mode supports only 1 target, but $targetCount were provided."
            )
            .leftNec
        else
          val red  = ghost.redDetector.value
          val blue = ghost.blueDetector.value
          (red.timeAndCount, blue.timeAndCount)
            .mapN: (redTC, blueTC) =>
              // Need to put the signalToNoiseAt into the individual detectors for the IT
              val itcRed: ItcGhostDetector  =
                ItcGhostDetector(redTC.copy(at = ghost.signalToNoiseAt), red.readMode, red.binning)
              val itcBlue: ItcGhostDetector =
                ItcGhostDetector(
                  blueTC.copy(at = ghost.signalToNoiseAt),
                  blue.readMode,
                  blue.binning
                )
              InstrumentMode
                .GhostSpectroscopy(ghost.resolutionMode,
                                   redDetector = itcRed,
                                   blueDetector = itcBlue
                )
                .rightNec
            .getOrElse(
              ItcQueryProblem
                .GenericError("GHOST only supports Time and Count exposure mode.")
                .leftNec
            )

      row match
        case ItcInstrumentConfig.GmosNorthSpectroscopy(grating, fpu, filter, etm, modeOverrides) =>
          val roi: Option[GmosRoi]     = modeOverrides.map(_.roi)
          val ccd: Option[GmosCcdMode] = modeOverrides.map(_.ccdMode)
          modeOverrides
            .map(_.centralWavelength.value)
            .map: (cw: Wavelength) =>
              InstrumentMode
                .GmosNorthSpectroscopy(etm,
                                       cw,
                                       grating,
                                       filter,
                                       GmosFpu.North(fpu.asRight),
                                       ccd,
                                       roi
                )
                .rightNec
            .getOrElse(ItcQueryProblem.MissingWavelength.leftNec)
        case ItcInstrumentConfig.GmosSouthSpectroscopy(grating, fpu, filter, etm, modeOverrides) =>
          val roi: Option[GmosRoi]     = modeOverrides.map(_.roi)
          val ccd: Option[GmosCcdMode] = modeOverrides.map(_.ccdMode)
          modeOverrides
            .map(_.centralWavelength.value)
            .map: (cw: Wavelength) =>
              InstrumentMode
                .GmosSouthSpectroscopy(etm,
                                       cw,
                                       grating,
                                       filter,
                                       GmosFpu.South(fpu.asRight),
                                       ccd,
                                       roi
                )
                .rightNec
            .getOrElse(ItcQueryProblem.MissingWavelength.leftNec)
        case ItcInstrumentConfig.Flamingos2Spectroscopy(disperser, filter, fpu, etm)             =>
          InstrumentMode
            .Flamingos2Spectroscopy(etm, disperser, filter, fpu)
            .rightNec
        case ItcInstrumentConfig.GmosNorthImaging(filter, etm)                                   =>
          InstrumentMode.GmosNorthImaging(etm, filter, none).rightNec
        case ItcInstrumentConfig.GmosSouthImaging(filter, etm)                                   =>
          InstrumentMode.GmosSouthImaging(etm, filter, none).rightNec
        case ItcInstrumentConfig.Igrins2Spectroscopy(etm)                                        =>
          InstrumentMode.Igrins2Spectroscopy(etm).rightNec
        case g: ItcInstrumentConfig.GhostIfu                                                     =>
          validateGhostMode(g, targetCount)
        case _                                                                                   =>
          ItcQueryProblem.UnsupportedMode.leftNec

  // We may consider adjusting this to consider small variations of RV identical for the
  // purpose of doing ITC calculations
  private given Hash[RadialVelocity] = Hash.by(_.rv.value)
  private given Hash[SourceProfile]  = Hash.fromUniversalHashCode
  private given Hash[TargetInput]    = Hash.by(x => (x.sourceProfile, x.radialVelocity))
  private given Hash[ItcTarget]      = Hash.by(x => (x.name.value, x.input))

  extension (targetIds: SortedSet[Target.Id])
    // assumes all targets are present
    def toAsterism(allTargets: TargetList): List[Target] =
      targetIds.toList.map(targetId => allTargets.get(targetId).map(_.target)).flattenOption

    def toItcTargets(allTargets: TargetList): EitherNec[ItcTargetProblem, NonEmptyList[ItcTarget]] =
      toAsterism(allTargets).toItcTargets

  extension (target: Target)
    def itcTarget: EitherNec[ItcTargetProblem, ItcTarget] =
      // ToOs have source profiles, but no RV, so we'll just use Zero
      val rv: RadialVelocity = TargetRV.getOption(target).getOrElse(RadialVelocity.Zero)
      ItcTarget(target.name, TargetInput(Target.sourceProfile.get(target), rv)).orItcProblem

  extension (asterism: List[Target])
    def toItcTargets: EitherNec[ItcTargetProblem, NonEmptyList[ItcTarget]] =
      asterism
        .traverse(_.itcTarget)
        .map(_.hashDistinct)
        .flatMap(
          _.toNel
            .toRightNec(ItcTargetProblem(None, ItcQueryProblem.GenericError(Constants.NoTargets)))
        )

object syntax extends syntax
