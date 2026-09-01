// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Eq
import cats.data.NonEmptySet
import cats.derived.*
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.EditableLabel
import explore.Icons
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Observation
import explore.model.display.given
import explore.model.syntax.all.*
import explore.render.*
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Program
import lucuma.core.model.TelluricType
import lucuma.core.syntax.all.*
import lucuma.core.util.CalculatedValue
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.LayeredIcon
import lucuma.react.fa.TextLayer
import lucuma.react.primereact.Button
import lucuma.react.primereact.Checkbox
import lucuma.react.primereact.Tag
import lucuma.react.primereact.hooks.all.*
import lucuma.react.primereact.tooltip.*
import lucuma.refined.*
import lucuma.schemas.model.ObservingMode
import lucuma.ui.components.TimeSpanView
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*

import scala.collection.immutable.SortedSet

final case class ObsBadge(
  obs:                   Observation,
  layout:                ObsBadge.Layout,
  selected:              Boolean = false,
  setStateCB:            Option[Observation.Id => ObservationWorkflowState => Callback] = none,
  setSubtitleCB:         Option[Option[NonEmptyString] => Callback] = none,
  setScienceBandCB:      Option[ScienceBand => Callback] = none,
  setTelluricTypeCB:     Option[TelluricType => Callback] = none,
  deleteCB:              Callback,
  cloneCB:               Option[Callback] = none,
  allocatedScienceBands: SortedSet[ScienceBand],
  associatedObss:        List[Observation] = List.empty,
  programId:             Program.Id,
  hasBlindOffset:        Boolean = false,
  focusedObs:            Option[Observation.Id] = none,
  readonly:              Boolean = false
) extends ReactFnProps(ObsBadge.component):
  val executionTime: CalculatedValue[Option[TimeSpan]] = obs.execution.digest.programTimeEstimate
  val isDisabled: Boolean                              = readonly || obs.isCalibration
  val isDisabledExecuted: Boolean                      = isDisabled || obs.isExecuted
  val nonEmptyAllocatedBands                           = NonEmptySet.fromSet(allocatedScienceBands)
  val scienceBandIsInvalid                             = obs.scienceBand.exists(b => !allocatedScienceBands.contains(b))
  val showScienceBand: Boolean                         =
    obs.calibrationRole.isEmpty && allocatedScienceBands.nonEmpty

  val telluricType =
    Option
      .unless(obs.isCalibration)(obs.observingMode.toOption.flatten)
      .flatten
      .flatMap(ObservingMode.telluricType.getOption)

object ObsBadge:
  private type Props = ObsBadge

  enum Section derives Eq:
    case None, Header, Detail

  final case class Layout(
    showTitle:         Boolean,
    showSubtitle:      Boolean,
    showConfiguration: Section,
    showConstraints:   Boolean
  ) derives Eq

  object Layout:
    val ObservationsTab: Layout = Layout(true, true, Section.Detail, true)
    val TargetsTab: Layout      = Layout(false, false, Section.Header, true)
    val ConstraintsTab: Layout  = Layout(true, false, Section.Detail, false)

  // Dropdown of TelluricType
  private enum TelluricSelection(val tag: String, val label: String) derives Eq:
    case Hot        extends TelluricSelection("hot", "Hot")
    case A0V        extends TelluricSelection("a0v", "A0V")
    case Solar      extends TelluricSelection("solar", "Solar")
    case Manual     extends TelluricSelection("manual", "Manual")
    case NoTelluric extends TelluricSelection("noTelluric", "None")

  private object TelluricSelection:
    given Enumerated[TelluricSelection] =
      Enumerated.from(Hot, A0V, Solar, Manual, NoTelluric).withTag(_.tag)

    given Display[TelluricSelection] = Display.byShortName(_.label)

    def fromTelluricType(tt: TelluricType): TelluricSelection = tt match
      case TelluricType.Hot        => Hot
      case TelluricType.A0V        => A0V
      case TelluricType.Solar      => Solar
      case TelluricType.Manual(_)  => Manual
      case TelluricType.NoTelluric => NoTelluric

    def toTelluricType(selection: TelluricSelection): Option[TelluricType] = selection match
      case Hot        => TelluricType.Hot.some
      case A0V        => TelluricType.A0V.some
      case Solar      => TelluricType.Solar.some
      case NoTelluric => TelluricType.NoTelluric.some
      case Manual     => none

  // TODO Make this a component similar to the one in the docs.
  private def renderEnumProgress[A: Enumerated](value: A): VdomNode = {
    val all = summon[Enumerated[A]].all
    <.progress(^.width := "100%", ^.max := all.length - 1, ^.value := all.indexOf(value))
  }

  private def obsIdentifier(obs: Observation): String =
    obs.reference.fold(s"[${obs.id.show}]")(ref => s"[${ref.observationIndex}]")

  // GHOST modes can carry a manually-set sky position
  private def configLabel(obs: Observation, shortName: String): String =
    if obs.hasSkyPosition then s"$shortName + Sky" else shortName

  // Unapproved gets an "X", since "U" is taken by Undefined
  private def stateLetter(state: ObservationWorkflowState): String =
    state match
      case ObservationWorkflowState.Inactive   => "I"
      case ObservationWorkflowState.Undefined  => "U"
      case ObservationWorkflowState.Unapproved => "X"
      case ObservationWorkflowState.Defined    => "D"
      case ObservationWorkflowState.Ready      => "R"
      case ObservationWorkflowState.Ongoing    => "O"
      case ObservationWorkflowState.Completed  => "C"

  private def stateTag(state: ObservationWorkflowState): VdomNode =
    <.span(
      Tag(
        value = stateLetter(state),
        rounded = true,
        clazz = ExploreStyles.ObsBadgeAssociatedObsState
      )
    ).withTooltip(content = state.shortName)

  // Daytime pinhole calibrations have no meaningful target, so we label them by role.
  private def badgeTitle(obs: Observation): String =
    obs.calibrationRole match
      case Some(CalibrationRole.DaytimePinhole) => "Daytime Pinhole"
      case _                                    => obs.title

  private val component = ScalaFnComponent[Props]: props =>
    for
      ctx     <- useContext(AppContext.ctx)
      menuRef <- usePopupMenuRef
    yield
      val obs    = props.obs
      val layout = props.layout

      val identifier: VdomNode = obs.reference.fold(<.span(obsIdentifier(obs))): ref =>
        <.span(obsIdentifier(obs)).withTooltip(content = s"${ref.label} (${obs.id})")

      val deleteButton =
        Button(
          text = true,
          clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObsDeleteButton,
          icon = Icons.Trash,
          tooltip = "Delete",
          onClickE = e => e.preventDefaultCB *> e.stopPropagationCB *> props.deleteCB
        ).small.unless(props.isDisabledExecuted)

      val duplicateButton =
        Button(
          text = true,
          clazz = ExploreStyles.ObsCloneButton,
          icon = Icons.Clone,
          tooltip = "Duplicate",
          onClickE = e => e.preventDefaultCB *> e.stopPropagationCB *> props.cloneCB.getOrEmpty
        ).small.unless(props.isDisabled)

      val scienceBandIcon =
        LayeredIcon(fixedWidth = true)(
          Icons.Circle,
          TextLayer(obs.scienceBand.map(b => (b.ordinal + 1).toString).getOrElse("-"),
                    inverse = false
          )
        )

      val scienceBandToolTip: String =
        val action =
          if (obs.scienceBand.isEmpty || props.scienceBandIsInvalid) "set" else "change"
        List(
          obs.scienceBand.map(_.longName).getOrElse("Science band not set").some,
          props.setScienceBandCB.map(_ => s"Click to $action")
        ).flatten
          .mkString("\n")

      val scienceBandButton =
        Button(
          text = true,
          clazz = ExploreStyles.ObsScienceBandButton,
          icon = scienceBandIcon,
          tooltip = scienceBandToolTip,
          onClickE = e =>
            // don't show menu if there is no callback defined
            e.preventDefaultCB *> e.stopPropagationCB *>
              menuRef.toggle(e).when(props.setScienceBandCB.isDefined).void
        )

      val header =
        <.div(ExploreStyles.ObsBadgeHeader)(
          <.div(ExploreStyles.ObsBadgeTargetAndId)(
            <.div(badgeTitle(obs)).when(layout.showTitle),
            <.div(obs.basicConfiguration.map(c => configLabel(obs, c.shortName)).getOrElse("-"))
              .when(layout.showConfiguration === Section.Header),
            <.div(
              ExploreStyles.ObsBadgeId,
              scienceBandButton.when(props.showScienceBand),
              identifier,
              props.cloneCB.whenDefined(using _ => duplicateButton),
              deleteButton
            )
          )
        )

      val meta = <.div(ExploreStyles.ObsBadgeMeta)(
        props.setSubtitleCB
          .map(setCB =>
            EditableLabel(
              value = obs.subtitle,
              mod = setCB,
              editOnClick = false,
              textClass = ExploreStyles.ObsBadgeSubtitle,
              inputClass = ExploreStyles.ObsBadgeSubtitleInput,
              addButtonLabel = "Add description",
              addButtonClass = ExploreStyles.ObsBadgeSubtitleAdd,
              leftButtonClass = ExploreStyles.ObsBadgeSubtitleEdit,
              rightButtonClass = ExploreStyles.ObsBadgeSubtitleDelete,
              readonly = props.isDisabledExecuted
            )
          )
          .whenDefined
          .when(layout.showSubtitle),
        renderEnumProgress(obs.workflow.state)
      )

      lazy val validationTooltip =
        if (obs.hasConfigurationRequestError)
          <.span(obs.workflow.value.validationErrors.head.messages.head)
        else
          <.div(
            obs.workflow.value.validationErrors
              .toTagMod(using
                ov =>
                  <.div(
                    ov.code.name + obs.severityOf(ov.code).acknowledgedSuffix,
                    <.ul(ov.messages.toList.toTagMod(using i => <.li(i)))
                  )
              )
          )

      lazy val validationIcon: VdomNode =
        obs.validationSeverity
          .map(severity =>
            <.span(validationSeverityIcon(severity)).withTooltip(content = validationTooltip)
          )
          .getOrElse(EmptyVdom)

      val telluricDropdown: Option[VdomNode] =
        (props.telluricType, props.setTelluricTypeCB).mapN: (telluricType, setCB) =>
          val current = TelluricSelection.fromTelluricType(telluricType)
          <.span(ExploreStyles.ObsStateSelectWrapper)(
            EnumDropdownView(
              id = NonEmptyString.unsafeFrom(s"obs-telluric-${obs.id}"),
              value = View[TelluricSelection](
                current,
                (f, cb) =>
                  val newValue = f(current)
                  TelluricSelection
                    .toTelluricType(newValue)
                    .map(setCB)
                    .getOrEmpty >> cb(current, newValue)
              ),
              exclude = Option
                // TODO: Don't support manual mode just yet
                .unless(current === TelluricSelection.Manual)(TelluricSelection.Manual)
                .toSet,
              disabledItems = Set(TelluricSelection.Manual),
              size = PlSize.Mini,
              clazz = ExploreStyles.ObsStateSelect,
              panelClass = ExploreStyles.ObsStateSelectPanel,
              disabled = props.isDisabledExecuted
            )
          )(
            // don't select the observation when changing the telluric type
            ^.onClick ==> { e => e.preventDefaultCB >> e.stopPropagationCB }
          )

      val telluricHelpIcon: Option[VdomNode] =
        (props.telluricType, props.setTelluricTypeCB).mapN: (_, _) =>
          HelpIcon("configuration/telluric-type.md".refined)

      React.Fragment(
        <.div(
          <.div(ExploreStyles.ObsBadge, ExploreStyles.ObsBadgeSelected.when(props.selected))(
            header,
            meta,
            <.div(ExploreStyles.ObsBadgeDescription)(
              <.span(ExploreStyles.ObsBadgeDescriptionTitles)(
                obs.observingModeSummary
                  .map(s => <.div(configLabel(obs, s.shortName)))
                  .whenDefined
                  .when(layout.showConfiguration === Section.Detail),
                <.div(obs.constraintsSummary).when(layout.showConstraints)
              ),
              <.span(Icons.LocationDot)
                .withTooltip(content = "Blind Offset")
                .when(props.hasBlindOffset)
            ),
            <.div(ExploreStyles.ObsBadgeExtra)(
              <.div(ExploreStyles.ObsBadgeExtraStatus)(
                props.setStateCB.map(setStatus =>
                  <.span(ExploreStyles.ObsStateSelectWrapper)(
                    EnumDropdownView(
                      id = NonEmptyString.unsafeFrom(s"obs-status-${obs.id}"),
                      value = View[ObservationWorkflowState](
                        obs.workflow.value.state,
                        (f, cb) =>
                          val oldValue = obs.workflow.value.state
                          val newValue = f(obs.workflow.value.state)
                          setStatus(props.obs.id)(newValue) >> cb(oldValue, newValue)
                      ),
                      size = PlSize.Mini,
                      clazz = ExploreStyles.ObsStateSelect,
                      panelClass = ExploreStyles.ObsStateSelectPanel,
                      disabled =
                        props.readonly || obs.workflow.isStale, // calibration workflows can be edited
                      exclude = obs.disabledStates
                    )
                  )(
                    // don't select the observation when changing the status
                    ^.onClick ==> { e => e.preventDefaultCB >> e.stopPropagationCB }
                  ).withOptionalTooltip(obs.workflow.staleTooltip)
                ),
                props.executionTime.value.map(t =>
                  TimeSpanView(t, tooltip = props.executionTime.staleTooltip)
                    .withMods(props.executionTime.staleClass)
                ),
                validationIcon,
                // Placed last so the grid auto-places them on the second row,
                // with the dropdown sharing the state dropdown's column.
                telluricDropdown,
                telluricHelpIcon
              ),
              <.div(ExploreStyles.ObsBadgeExtraAssociated)(
                props.associatedObss
                  .map: childObs =>
                    val selected: Boolean = props.focusedObs.contains_(childObs.id)

                    val currentState: ObservationWorkflowState = childObs.workflow.value.state

                    Button(
                      clazz = ExploreStyles.ObsBadgeAssociatedObs |+|
                        ExploreStyles.ObsBadgeSelectedAssociatedObs.when_(selected),
                      onClickE = linkOverride(
                        focusObs(props.programId, childObs.id.some, ctx)
                      ),
                      severity = Button.Severity.Secondary
                    ).withMods(
                      // TODO: Enable when the odb really supports changing the state.
                      Checkbox(
                        checked = currentState === ObservationWorkflowState.Ready,
                        variant = Checkbox.Variant.Filled,
                        clazz = ExploreStyles.ObsBadgeAssociatedObsCheckbox,
                        disabled = true
                      )(^.onClick ==> (e => e.preventDefaultCB *> e.stopPropagationCB)),
                      stateTag(currentState),
                      <.span(ExploreStyles.ObsBadgeAssociatedObsContent)(
                        <.span(badgeTitle(childObs)),
                        <.span(
                          obsIdentifier(childObs),
                          childObs.execution.digest.programTimeEstimate.value
                            .map(t => TimeSpanView(t).withMods(^.marginLeft := "0.5em"))
                        )
                      )
                    ).compact
                  .toTagMod
              ).when(props.associatedObss.nonEmpty)
            )
          )
        ),
        (props.nonEmptyAllocatedBands, props.setScienceBandCB).mapN: (bs, cb) =>
          ScienceBandPopupMenu(
            currentBand = obs.scienceBand,
            allocatedScienceBands = bs,
            onSelect = cb,
            menuRef = menuRef
          )
      )
