// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.ExploreModelValidators
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.geom.OffsetGenerator
import lucuma.core.math.*
import lucuma.core.util.NewBoolean
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Checkbox
import lucuma.react.primereact.Dialog
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.*

case class OffsetGeneratorEditor(
  id:       NonEmptyString,
  label:    String,
  value:    View[Option[OffsetGenerator]],
  readonly: Boolean
) extends ReactFnProps(OffsetGeneratorEditor)
//   offsets:     View[List[Offset]],
//   onUpdate:    List[Offset] => Callback,
//   pointCount:  PosInt,
//   defaultSize: Angle,
//   readOnly:    Boolean = false
// )(using L: Logger[IO])
//     extends ReactFnProps[OffsetEditor](OffsetEditor.component):
//   given Logger[IO] = L

//   def hasOffsets = offsets.get.nonEmpty

object OffsetGeneratorEditor
    extends ReactFnComponent[OffsetGeneratorEditor]({ props =>
      object IsOpen extends NewBoolean

      // val offsetReadOnly = props.readonly || editState.get === ConfigEditState.View
      // val offsetsCount   = offsets(props.observingMode).get.size
      val offsetsText: String = "<count> offsets"
      //   if (offsetsCount == 0) "No offsets"
      //   else if (offsetsCount == 1) "1 offset"
      //   else s"$offsetsCount offsets"

      val valueOpt: Option[View[OffsetGenerator]]           = props.value.toOptionView
      val randomOpt: Option[View[OffsetGenerator.Random]]   =
        valueOpt.flatMap(_.zoom(OffsetGenerator.random).toOptionView)
      val spiralOpt: Option[View[OffsetGenerator.Spiral]]   =
        valueOpt.flatMap(_.zoom(OffsetGenerator.spiral).toOptionView)
      val uniformOpt: Option[View[OffsetGenerator.Uniform]] =
        valueOpt.flatMap(_.zoom(OffsetGenerator.uniform).toOptionView)

      for
        isDialogOpen <- useState(IsOpen.False)
        offsets      <- useEffectResultWithDeps(props.value.get): og =>
                          og.foldMap(_.generate[IO](1.refined).map(_.some))
                            .handleErrorWith: e =>
                              // TODO Toast
                              IO.println(s"Error generating offsets: ${e.getMessage}") >>
                                IO.pure(none)
        showNumbers  <- useStateView(false)
        showArrows   <- useStateView(true)
      yield
        val gridSection: VdomNode =
          offsets.value.value.renderPot: readyOffsets =>
            readyOffsets.map: o =>
              OffsetGridDisplay(
                o.toList,
                showNumbers = showNumbers.get || o.size < 30,
                showArrows = showArrows.get
              )

        val viewOptionsSection: VdomNode =
          ReactFragment(
            <.div(OffsetEditorStyles.FormRow)(
              <.div(OffsetEditorStyles.SmallCheckbox)(
                Checkbox(
                  inputId = "show-numbers",
                  checked = showNumbers.get || offsets.value.toOption.flatten.exists(_.size < 30),
                  onChange = checked => showNumbers.set(checked)
                ),
                <.label(^.htmlFor := "show-numbers", " Offset numbers")
              ),
              <.div(OffsetEditorStyles.SmallCheckbox)(
                Checkbox(
                  inputId = "show-arrows",
                  checked = showArrows.get,
                  onChange = checked => showArrows.set(checked)
                ),
                <.label(^.htmlFor := "show-arrows", " Show progression")
              )
            )
          )

        React.Fragment(
          FormLabel(htmlFor = props.id)(props.label),
          <.div(ExploreStyles.FlexContainer)(
            Button(
              icon = if props.readonly then Icons.Eye else Icons.Edit,
              text = true,
              severity = if props.readonly then Button.Severity.Info else Button.Severity.Secondary,
              clazz = ExploreStyles.OffsetEditorButton,
              onClickE = _ => isDialogOpen.setState(IsOpen.True)
            ).mini.compact
              .withMods(
                ^.id    := props.id.value,
                ^.title := (if props.readonly then s"View ${props.label}"
                            else s"Edit ${props.label}")
              ),
            <.span(ExploreStyles.OffsetsCount)(s"($offsetsText)")
          ),
          Dialog(
            id = s"${props.id.value}-dialog",
            visible = isDialogOpen.value,
            onHide = isDialogOpen.setState(IsOpen.False),
            header = if props.readonly then s"View ${props.label}" else props.label,
            modal = true,
            resizable = false,
            clazz = ExploreStyles.OffsetsEditorDialog
          ) // (<.div(props.value.get.toString))
          // <.div
          // OffsetEditor(
          //   if (offsetReadOnly) offsets(props.observingMode) else localOffsets,
          //   offsets => localOffsets.set(offsets).unless_(offsetReadOnly),
          //   props.requirementsExposureTimeMode match {
          //     case Some(ExposureTimeMode.TimeAndCountMode(_, c, _)) => c
          //     case Some(ExposureTimeMode.SignalToNoiseMode(_, _))   => 1.refined // fixme
          //     case _                                                => 1.refined
          //   },
          //   OffsetRadius,
          //   readOnly = offsetReadOnly
          // ),
          (
            if (props.readonly)
              <.div(OffsetEditorStyles.ContentReadOnly)(
                gridSection,
                viewOptionsSection
              )
            else
              <.div(OffsetEditorStyles.Content)(
                gridSection,
                <.div(OffsetEditorStyles.GridControls)(
                  React.Fragment(
                    <.h4("Generator Parameters"),
                    <.div(OffsetEditorStyles.FormRow)(
                      <.label(^.htmlFor := "grid-type", "Type:"),
                      EnumDropdown(
                        id = "grid-type".refined,
                        value = GridType.fromOffsetGenerator(props.value.get),
                        onChange = gt => props.value.set(gt.init),
                        placeholder = "Select grid type"
                      )
                    ),
                    randomOpt.map { random =>
                      ReactFragment(
                        <.div(OffsetEditorStyles.FormRow)(
                          <.label(^.htmlFor := "random-size", "Size (arcsec):"),
                          FormInputTextView(
                            id = "random-size".refined,
                            value = random.zoom(OffsetGenerator.Random.size),
                            validFormat = ExploreModelValidators.decimalArcsecondsValidWedge,
                            placeholder = "0.0"
                          )
                        ),
                        <.div(OffsetEditorStyles.FormRow)(
                          <.label(^.htmlFor := "random-size", "Center (arcsec):"),
                          OffsetInput(
                            id = "random-center".refined,
                            offset = random.zoom(OffsetGenerator.Random.center),
                            readonly = props.readonly
                          )
                        )
                      )
                    },
                    spiralOpt.map { spiral =>
                      ReactFragment(
                        <.div(OffsetEditorStyles.FormRow)(
                          <.label(^.htmlFor := "spiral-size", "Size (arcsec):"),
                          FormInputTextView(
                            id = "spiral-size".refined,
                            value = spiral.zoom(OffsetGenerator.Spiral.size),
                            validFormat = ExploreModelValidators.decimalArcsecondsValidWedge,
                            placeholder = "0.0"
                          )
                        ),
                        <.div(OffsetEditorStyles.FormRow)(
                          <.label(^.htmlFor := "spiral-center", "Center (arcsec):"),
                          OffsetInput(
                            id = "spiral-center".refined,
                            offset = spiral.zoom(OffsetGenerator.Spiral.center),
                            readonly = props.readonly
                          )
                        )
                      )
                    },
                    uniformOpt.map { uniform =>
                      ReactFragment(
                        <.div(OffsetEditorStyles.FormRow)(
                          <.label(^.htmlFor := "uniform-corner-a", "Corner A (arcsec):"),
                          OffsetInput(
                            id = "uniform-corner-a".refined,
                            offset = uniform.zoom(OffsetGenerator.Uniform.cornerA),
                            readonly = props.readonly
                          )
                        ),
                        <.div(OffsetEditorStyles.FormRow)(
                          <.label(^.htmlFor := "uniform-corner-b", "Corner B (arcsec):"),
                          OffsetInput(
                            id = "uniform-corner-b".refined,
                            offset = uniform.zoom(OffsetGenerator.Uniform.cornerB),
                            readonly = props.readonly
                          )
                        )
                      )
                    }
                  )
                )
              )
          )
        )
    })
