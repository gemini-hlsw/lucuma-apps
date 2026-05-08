// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model

import cats.Eq
import cats.Order.given
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.HCursor
import io.circe.generic.semiauto.*
import io.circe.refined.given
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Attachment
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.TimingWindow
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.syntax.display.*
import lucuma.core.util.Timestamp
import lucuma.schemas.decoders.given
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.ObservingMode
import monocle.Focus
import org.typelevel.cats.time.*

import java.time.Instant
import scala.collection.immutable.SortedSet

case class ObsSummary(
  obsId:              Observation.Id,
  programId:          Program.Id,
  title:              String,
  subtitle:           Option[NonEmptyString],
  instrument:         Instrument,
  constraints:        ConstraintSet,
  timingWindows:      List[TimingWindow],
  attachmentIds:      SortedSet[Attachment.Id],
  observingMode:      Option[ObservingMode],
  observationTime:    Option[Instant],
  posAngleConstraint: PosAngleConstraint,
  obsReference:       Option[ObservationReference],
  workflowState:      ObservationWorkflowState
) derives Eq:
  lazy val configurationSummary: Option[String] =
    observingMode
      .map(_.toBasicConfiguration)
      .flatMap:
        case BasicConfiguration.GmosNorthLongSlit(grating, _, fpu, _)         =>
          s"${grating.shortName} ${fpu.shortName}".some
        case BasicConfiguration.GmosSouthLongSlit(grating, _, fpu, _)         =>
          s"${grating.shortName} ${fpu.shortName}".some
        case BasicConfiguration.GmosNorthImaging(filters)                     =>
          filters.map(_.shortName).toList.mkString(", ").some
        case BasicConfiguration.GmosSouthImaging(filters)                     =>
          filters.map(_.shortName).toList.mkString(", ").some
        case BasicConfiguration.Flamingos2LongSlit(disperser, _, fpu)         =>
          s"${disperser.shortName} ${fpu.shortName}".some
        case BasicConfiguration.Igrins2LongSlit                               =>
          none
        case BasicConfiguration.GhostIfu(resolutionMode, _, _, _)             =>
          resolutionMode.shortName.some
        case BasicConfiguration.GnirsLongSlit(filter, fpu, acqMirror, camera) =>
          // For Gnirs Spectroscopy we should return this pattern:
          // GNIRS <CAM> <GRATING> @ <WAVELENGTH> <PRISM IF NOT MIRROR> <FPU><IF Altair AO:mode>
          // For example:
          // GNIRS SB 32 l/mm @ 2.23um 1" slit
          // GNIRS SB 32 l/mm @ 2.23um SXD 0.30" slit
          // GNIRS LB 111 l/mm @ 1.67um LR-IFU AO:NGS
          // GNIRS LB 111 l/mm @ 1.67um SXD 0.15" slit AO:LGS
          val mirrorSummary: String = acqMirror match
            case GnirsAcquisitionMirrorMode.In                              => ""
            case GnirsAcquisitionMirrorMode.Out(prism, grating, wavelength) =>
              val prismSummary: String      = prism match
                case GnirsPrism.Mirror => ""
                case p                 => s" ${p.shortName}"
              val wavelengthSummary: String =
                f"${wavelength.value.toMicrometers.value}%.2fµm"
              s" ${grating.longName} @ $wavelengthSummary$prismSummary"
          s"${camera.shortName}$mirrorSummary ${fpu.shortName} slit".some
          // For Gnirs Imaging we should return this pattern:
          // s"${filter.shortName} ${fpu.shortName} ${acqMirror.shortName} ${camera.shortName}".some
          // GNIRS Imaging:
          // GNIRS <CAM> <FILTER LIST>-band imaging <IF Altair AO: mode>
          // For example:
          // GNIRS SB J/H/K-band imaging
          // GNIRS LB K-band imaging AO:LGS+P1

  lazy val instrumentConfigurationSummary: String =
    s"${instrument.shortName} ${configurationSummary.orEmpty}"

  lazy val constraintsSummary: String =
    s"${constraints.imageQuality.toImageQuality.label} ${constraints.cloudExtinction.toCloudExtinction.label} ${constraints.skyBackground.label} ${constraints.waterVapor.label}"

  lazy val refAndId: String =
    obsReference.fold(obsId.shortName)(ref => s"${ref.label} (${obsId.shortName})")

object ObsSummary:
  val obsId              = Focus[ObsSummary](_.obsId)
  val programId          = Focus[ObsSummary](_.programId)
  val title              = Focus[ObsSummary](_.title)
  val subtitle           = Focus[ObsSummary](_.subtitle)
  val instrument         = Focus[ObsSummary](_.instrument)
  val constraints        = Focus[ObsSummary](_.constraints)
  val timingWindows      = Focus[ObsSummary](_.timingWindows)
  val attachmentIds      = Focus[ObsSummary](_.attachmentIds)
  val observingMode      = Focus[ObsSummary](_.observingMode)
  val observationTime    = Focus[ObsSummary](_.observationTime)
  val posAngleConstraint = Focus[ObsSummary](_.posAngleConstraint)
  val obsReference       = Focus[ObsSummary](_.obsReference)

  private case class AttachmentIdWrapper(id: Attachment.Id)
  private object AttachmentIdWrapper:
    given Decoder[AttachmentIdWrapper] = deriveDecoder

  given Decoder[ObsSummary] = Decoder.instance: c =>
    for
      id                 <- c.get[Observation.Id]("id")
      programId          <- c.downField("program").get[Program.Id]("id")
      title              <- c.get[String]("title")
      subtitle           <- c.get[Option[NonEmptyString]]("subtitle")
      instrument         <- c.get[Option[Instrument]]("instrument")
      constraints        <- c.get[ConstraintSet]("constraintSet")
      timingWindows      <- c.get[List[TimingWindow]]("timingWindows")
      attachmentIds      <- c.get[List[AttachmentIdWrapper]]("attachments")
      observingMode      <- c.get[Option[ObservingMode]]("observingMode")
      observationTime    <- c.get[Option[Timestamp]]("observationTime")
      posAngleConstraint <- c.get[PosAngleConstraint]("posAngleConstraint")
      obsReference       <-
        c.get[Option[HCursor]]("reference")
          .map(_.map(_.get[Option[ObservationReference]]("label")).sequence.map(_.flatten))
          .sequence
          .flatten
      workflowState      <-
        c.downField("workflow").downField("value").get[ObservationWorkflowState]("state")
    yield ObsSummary(
      id,
      programId,
      title,
      subtitle,
      instrument.getOrElse(Instrument.VisitorSouth),
      constraints,
      timingWindows,
      SortedSet.from(attachmentIds.map(_.id)),
      observingMode,
      observationTime.map(_.toInstant),
      posAngleConstraint,
      obsReference,
      workflowState
    )
