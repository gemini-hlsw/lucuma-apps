// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Eq
import cats.derived.*
import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.util.NewBoolean
import lucuma.core.util.NewType
import lucuma.core.util.TimeSpan
import lucuma.react.SizePx
import lucuma.react.common.*
import lucuma.react.primereact.Tag
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.tooltip.*
import lucuma.react.syntax.*
import lucuma.react.table.RowId
import lucuma.schemas.model.PeakPixel
import lucuma.schemas.model.enums.StepExecutionState
import lucuma.ui.LucumaIcons
import lucuma.ui.LucumaStyles
import lucuma.ui.utils.Render
import lucuma.ui.utils.zipWithMappedIndex

final case class SelectedRowId(visitId: Option[Visit.Id], stepId: Step.Id) derives Eq

object SelectedRowId:
  def forFutureStep(stepId: Step.Id): SelectedRowId =
    SelectedRowId(none, stepId)

object SequenceRowHeight:
  val Regular: SizePx   = 25.toPx
  val WithExtra: SizePx = 60.toPx

/**
 * What a header row's content is rendered with: the sequence edit contexts and, for an expanded
 * visit, the buttons acting on its runs (empty otherwise).
 */
case class SequenceHeaderContext[D](editContexts: SequenceEditContexts[D], visitRunButtons: TagMod)

/**
 * Row id of a visit's run (a header row grouping its steps): `run:<visit id>:<run index>:<type>`.
 * The type lets the table header cell tell acquisition runs from science ones; the `run:` prefix
 * and the strict parsing keep any other row id from being mistaken for a run.
 */
object VisitRunRowId:
  private val Prefix: String = "run"

  def apply(visitId: Visit.Id, runIndex: Int, sequenceType: SequenceType): RowId =
    RowId(s"$Prefix:$visitId:$runIndex:${sequenceType.tag}")

  def sequenceType(rowId: RowId): Option[SequenceType] =
    rowId.value.split(':') match
      case Array(Prefix, _, _, tag) => SequenceType.values.find(_.tag === tag)
      case _                        => none

object StepIndex extends NewType[PosInt]:
  val One: StepIndex = StepIndex(PosInt.unsafeFrom(1))
type StepIndex = StepIndex.Type

object IsEditEnabled extends NewBoolean
type IsEditEnabled = IsEditEnabled.Type

object IsEditing extends NewBoolean
type IsEditing = IsEditing.Type

object IsEditInFlight extends NewBoolean
type IsEditInFlight = IsEditInFlight.Type

private def renderStepType(icon: VdomNode, tooltip: String): VdomNode =
  <.span(icon).withTooltip(content = tooltip, showDelay = 100, position = Tooltip.Position.Bottom)

extension (instrument: Instrument)
  def isSequenceEditable: Boolean =
    instrument match
      case Instrument.GmosNorth | Instrument.GmosSouth | Instrument.Flamingos2 |
          Instrument.Igrins2 | Instrument.Gnirs | Instrument.Ghost =>
        true
      case _ => false

  /**
   * Instruments not listed here (eg: GMOS-N, GMOS-S, Flamingos2) only accept integer exposure
   * times.
   */
  def acceptsDecimalExposureTime: Boolean =
    instrument match
      case Instrument.Gnirs | Instrument.Igrins2 | Instrument.Ghost => true
      case _                                                        => false

  def exposureTimeFractionDigits: Int =
    if instrument.acceptsDecimalExposureTime then 2 else 0

  def formatExposureTime(exposureTime: TimeSpan): NonEmptyString =
    NonEmptyString.unsafeFrom:
      exposureTime.toSeconds
        .setScale(instrument.exposureTimeFractionDigits, BigDecimal.RoundingMode.HALF_UP)
        .toString

extension (stepTypeDisplay: StepTypeDisplay)
  private def icon: VdomNode =
    stepTypeDisplay match
      case StepTypeDisplay.Bias   => SequenceIcons.StepType.Bias
      case StepTypeDisplay.Dark   => SequenceIcons.StepType.Dark
      case StepTypeDisplay.Arc    => SequenceIcons.StepType.Arc
      case StepTypeDisplay.Flat   => SequenceIcons.StepType.Flat
      case StepTypeDisplay.Object => SequenceIcons.StepType.Object

given Render[StepTypeDisplay] = Render.by: stepType =>
  renderStepType(stepType.icon, stepType.name)

given Render[Option[DatasetQaState]] = Render.by: qaState =>
  LucumaIcons.Circle.withClass:
    qaState match
      case Some(DatasetQaState.Pass)   => LucumaStyles.IndicatorOK
      case Some(DatasetQaState.Usable) => LucumaStyles.IndicatorWarning
      case Some(DatasetQaState.Fail)   => LucumaStyles.IndicatorFail
      case None                        => LucumaStyles.IndicatorUnknown

given Render[StepExecutionState] = Render.by:
  case StepExecutionState.NotStarted | StepExecutionState.Completed =>
    EmptyVdom
  case other @ (StepExecutionState.Ongoing | StepExecutionState.Aborted |
      StepExecutionState.Stopped | StepExecutionState.Abandoned) =>
    Tag(
      other match
        case StepExecutionState.Ongoing   => "Ongoing"
        case StepExecutionState.Aborted   => "Aborted"
        case StepExecutionState.Stopped   => "Stopped Early"
        case StepExecutionState.Abandoned => "Abandoned",
      severity = other match
        case StepExecutionState.Ongoing | StepExecutionState.Stopped => Tag.Severity.Info
        case _                                                       => Tag.Severity.Danger
    )

extension [D, R <: SequenceRow[D]](list: List[R])
  /* Zip list with `StepIndex` and return the indexed list and the next index */
  def zipWithStepIndex(
    initial: StepIndex = StepIndex(PosInt.unsafeFrom(1))
  ): (List[(R, StepIndex)], StepIndex) =
    (list.zipWithMappedIndex(index => initial.modifyValue(i => PosInt.unsafeFrom(i.value + index))),
     initial.modifyValue(i => PosInt.unsafeFrom(i.value + list.size))
    )

extension [D](instrumentConfig: D)
  def shouldShowAcquisitionSn: Boolean =
    instrumentConfig match
      case gmos.DynamicConfig.GmosNorth(_, _, _, _, _, _, None)                       => true
      case gmos.DynamicConfig.GmosSouth(_, _, _, _, _, _, None)                       => true
      case Flamingos2DynamicConfig(_, _, Flamingos2FpuMask.Imaging, _, _, _, _, _, _) => true
      // GNIRS acquisition images use the acquisition mirror (FPU) and decker. There may be
      // more than one when the acquisition has offsets.
      case GnirsDynamicConfig(decker = decker, fpu = fpu)                             =>
        decker === GnirsDecker.Acquisition && fpu === GnirsFpu.Other(GnirsFpuOther.Acquisition)
      case _                                                                          => false

extension [D](step: Step[D])
  def getSignalToNoise(
    signalToNoise: D => Option[SignalToNoise]
  ): Option[SignalToNoiseValue] =
    step.observeClass match
      case ObserveClass.Acquisition if step.instrumentConfig.shouldShowAcquisitionSn =>
        signalToNoise(step.instrumentConfig)
          .map(SignalToNoiseValue.Value.apply)
          .orElse(SignalToNoiseValue.Saturated.some)
      case ObserveClass.Science                                                      =>
        signalToNoise(step.instrumentConfig).map(SignalToNoiseValue.Value(_))
      case _                                                                         =>
        none

  // The ODB reports no peak pixel flux at all for a saturated source.
  def getPeakPixel(peakPixel: D => Option[PeakPixel]): Option[PeakPixel] =
    val shows = step.observeClass match
      case ObserveClass.Acquisition => step.instrumentConfig.shouldShowAcquisitionSn
      case ObserveClass.Science     => true
      case _                        => false
    Option.when(shows)(peakPixel(step.instrumentConfig)).flatten
