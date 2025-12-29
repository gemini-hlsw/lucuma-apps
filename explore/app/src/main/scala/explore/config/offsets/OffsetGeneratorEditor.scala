// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.effect.IO
import cats.syntax.all.*
import cats.tests.Helpers.Hsh.O
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
import lucuma.core.util.Display
import lucuma.core.util.NewBoolean
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Checkbox
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.Divider
import lucuma.refined.*
import lucuma.schemas.model.TelescopeConfigGenerator
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
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
                    randomOpt.map: random =>
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
//               gridType.get match {
//                 case GridType.Rectangular =>
//                   ReactFragment(
//                     <.div(
//                       OffsetEditorStyles.FormRow,
//                       <.label("Dimensions:"),
//                       <.div(
//                         s"${rectParams.get.rows.value} × ${rectParams.get.cols.value} (for ${props.pointCount.value} steps)"
//                       )
//                     ),
//                     <.div(
//                       OffsetEditorStyles.FormRow,
//                       <.label(^.htmlFor := "rect-step-p", "p step (arcsec):"),
//                       FormInputTextView(
//                         id = "rect-step-p".refined,
//                         value = rectParams.zoom(RectangularParams.stepP),
//                         validFormat = decimalArcsecondsValidWedge,
//                         placeholder = "0.0"
//                       )
//                     ),
//                     <.div(
//                       OffsetEditorStyles.FormRow,
//                       <.label(^.htmlFor := "rect-step-q", "q step (arcsec):"),
//                       FormInputTextView(
//                         id = "rect-step-q".refined,
//                         value = rectParams.zoom(RectangularParams.stepQ),
//                         validFormat = decimalArcsecondsValidWedge,
//                         placeholder = "0.0"
//                       )
//                     )
//                   )
//                 case GridType.Spiral      =>
//                   ReactFragment(
//                     <.div(
//                       OffsetEditorStyles.FormRow,
//                       <.label(^.htmlFor := "spiral-size", "Size (arcsec):"),
//                       FormInputTextView(
//                         id = "spiral-size".refined,
//                         value = spiralParams.zoom(SpiralParams.size),
//                         validFormat = decimalArcsecondsValidWedge,
//                         placeholder = "0.0"
//                       )
//                     ),
//                     <.div(
//                       OffsetEditorStyles.FormRow,
//                       <.label(s"Count:"),
//                       <.label(ExploreStyles.OffsetsCount, props.pointCount.value)
//                     )
//                   )
//                 case GridType.Random      =>
//                   ReactFragment(
//                     <.div(
//                       OffsetEditorStyles.FormRow,
//                       <.label(^.htmlFor := "random-size", "Size (arcsec):"),
//                       FormInputTextView(
//                         id = "random-size".refined,
//                         value = randomParams.zoom(RandomParams.size),
//                         validFormat = decimalArcsecondsValidWedge,
//                         placeholder = "0.0"
//                       )
//                     ),
//                     <.div(
//                       OffsetEditorStyles.FormRow,
//                       <.label(s"Count:"),
//                       <.label(ExploreStyles.OffsetsCount, props.pointCount.value)
//                     )
//                   )
//               },
//               gridType.get match {
//                 case GridType.Rectangular => EmptyVdom
//                 case _                    =>
//                   <.div(
//                     OffsetEditorStyles.FormRow,
//                     <.label("Refresh:"),
//                     Button(
//                       text = false,
//                       icon = Icons.ArrowsRepeat,
//                       severity = Button.Severity.Success,
//                       clazz = ExploreStyles.OffsetRegenerate
//                       // onClick = generateCurrentGrid(params, props.pointCount, updateOffsets)
//                     ).mini.compact
//                   )
//               },
//               Divider(),
//               viewOptionsSection
                      )
                  )
                )
              )
          )
        )
    })

// object OffsetEditor {
//   type Props = OffsetEditor

//   given Reusability[GridParams] = Reusability.byEq

//   given Display[GridType] = Display.byShortName[GridType](_.tag.capitalize)

//   private def squareGridDimension(pointCount: PosInt): PosInt =
//     val count = pointCount.value
//     val side  = math.ceil(math.sqrt(count.toDouble)).toInt
//     PosInt.from(side).getOrElse(1.refined)

//   private def generateCurrentGrid(
//     currentParams: GridParams,
//     pointCount:    PosInt,
//     updatePreview: List[Offset] => Callback
//   )(using Logger[IO]): Callback =
//   currentParams match {
//     case r: RectangularParams =>
//       val dim     = squareGridDimension(pointCount)
//       val newGrid = grid(dim, dim, r.stepP, r.stepQ)
//       updatePreview(newGrid.toList)

//     case s: SpiralParams =>
//       spiral[IO](pointCount, s.size)
//         .flatMap(offsets => updatePreview(offsets.toList).to[IO])
//         .runAsync

//     case r: RandomParams =>
//       random[IO](pointCount, r.size)
//         .flatMap(offsets => updatePreview(offsets.toList).to[IO])
//         .runAsync
//   }

//   val component = ScalaFnComponent[Props]: props =>
//     // import props.given

//     val dim = squareGridDimension(props.pointCount)

//     for {
//       rectParams     <- useStateView(RectangularParams(dim, dim, props.defaultSize, props.defaultSize))
//       spiralParams   <- useStateView(SpiralParams(props.defaultSize))
//       randomParams   <- useStateView(RandomParams(props.defaultSize))
//       gridType       <- useStateView(GridType.Random)
//       previewOffsets <- useState(props.offsets.get.some)
//       showNumbers    <- useStateView(false)
//       showArrows     <- useStateView(true)
//       isInitialMount <- useState(true)
//       params          = gridType.get match {
//                           case GridType.Rectangular => rectParams.get
//                           case GridType.Spiral      => spiralParams.get
//                           case GridType.Random      => randomParams.get
//                         }
//       updateOffsets   =
//         (offsets: List[Offset]) => previewOffsets.setState(offsets.some) *> props.onUpdate(offsets)
//       // _              <- useEffectOnMount {
//       //                     generateCurrentGrid(params, props.pointCount, updateOffsets)
//       //                       .unless_(props.hasOffsets)
//       //                   }
//       // _              <- useEffectWithDeps(params): params =>
//       //                     isInitialMount.setState(false) *>
//       //                       generateCurrentGrid(params, props.pointCount, updateOffsets)
//       //                         .unless_(isInitialMount.value && props.hasOffsets)
//       _              <- useEffectWithDeps(props.pointCount): pointCount =>
//                           val dim = squareGridDimension(pointCount)
//                           rectParams.mod(_.copy(rows = dim, cols = dim))
//       resize         <- useResizeDetector
//     } yield
//       val size = (resize.width, resize.height).mapN(_.min(_)).map(PosInt.from).flatMap(_.toOption)

//       val viewOptionsSection = ReactFragment(
//         <.div(
//           OffsetEditorStyles.FormRow,
//           <.div(
//             OffsetEditorStyles.SmallCheckbox,
//             Checkbox(
//               inputId = "show-numbers",
//               checked = showNumbers.get || previewOffsets.value.exists(_.size < 30),
//               onChange = checked => showNumbers.set(checked)
//             ),
//             <.label(^.htmlFor := "show-numbers", " Offset numbers")
//           ),
//           <.div(
//             OffsetEditorStyles.SmallCheckbox,
//             Checkbox(
//               inputId = "show-arrows",
//               checked = showArrows.get,
//               onChange = checked => showArrows.set(checked)
//             ),
//             <.label(^.htmlFor := "show-arrows", " Show progression")
//           )
//         )
//       )

//       if (props.readOnly)
//         <.div(
//           OffsetEditorStyles.ContentReadOnly,
//           <.div(
//             OffsetEditorStyles.GridDisplay,
//             (previewOffsets.value, size).mapN((o, s) =>
//               OffsetGridDisplay(o,
//                                 svgSize = s,
//                                 showNumbers = showNumbers.get || o.size < 30,
//                                 showArrows = showArrows.get
//               )
//             )
//           ).withRef(resize.ref),
//           viewOptionsSection
//         )
//       else
//         <.div(
//           OffsetEditorStyles.Content,
//           <.div(
//             OffsetEditorStyles.GridDisplay,
//             (previewOffsets.value, size).mapN((o, s) =>
//               OffsetGridDisplay(o,
//                                 svgSize = s,
//                                 showNumbers = showNumbers.get || o.size < 30,
//                                 showArrows = showArrows.get
//               )
//             )
//           ).withRef(resize.ref),
//           <.div(
//             OffsetEditorStyles.GridControls,
//             ReactFragment(
//               <.h4("Generator Parameters"),
//               <.div(
//                 OffsetEditorStyles.FormRow,
//                 <.label(^.htmlFor := "grid-type", "Type:"),
//                 FormEnumDropdownView(
//                   id = "grid-type".refined,
//                   value = gridType,
//                   placeholder = "Select grid type"
//                 )
//               ),
//               gridType.get match {
//                 case GridType.Rectangular =>
//                   ReactFragment(
//                     <.div(
//                       OffsetEditorStyles.FormRow,
//                       <.label("Dimensions:"),
//                       <.div(
//                         s"${rectParams.get.rows.value} × ${rectParams.get.cols.value} (for ${props.pointCount.value} steps)"
//                       )
//                     ),
//                     <.div(
//                       OffsetEditorStyles.FormRow,
//                       <.label(^.htmlFor := "rect-step-p", "p step (arcsec):"),
//                       FormInputTextView(
//                         id = "rect-step-p".refined,
//                         value = rectParams.zoom(RectangularParams.stepP),
//                         validFormat = decimalArcsecondsValidWedge,
//                         placeholder = "0.0"
//                       )
//                     ),
//                     <.div(
//                       OffsetEditorStyles.FormRow,
//                       <.label(^.htmlFor := "rect-step-q", "q step (arcsec):"),
//                       FormInputTextView(
//                         id = "rect-step-q".refined,
//                         value = rectParams.zoom(RectangularParams.stepQ),
//                         validFormat = decimalArcsecondsValidWedge,
//                         placeholder = "0.0"
//                       )
//                     )
//                   )
//                 case GridType.Spiral      =>
//                   ReactFragment(
//                     <.div(
//                       OffsetEditorStyles.FormRow,
//                       <.label(^.htmlFor := "spiral-size", "Size (arcsec):"),
//                       FormInputTextView(
//                         id = "spiral-size".refined,
//                         value = spiralParams.zoom(SpiralParams.size),
//                         validFormat = decimalArcsecondsValidWedge,
//                         placeholder = "0.0"
//                       )
//                     ),
//                     <.div(
//                       OffsetEditorStyles.FormRow,
//                       <.label(s"Count:"),
//                       <.label(ExploreStyles.OffsetsCount, props.pointCount.value)
//                     )
//                   )
//                 case GridType.Random      =>
//                   ReactFragment(
//                     <.div(
//                       OffsetEditorStyles.FormRow,
//                       <.label(^.htmlFor := "random-size", "Size (arcsec):"),
//                       FormInputTextView(
//                         id = "random-size".refined,
//                         value = randomParams.zoom(RandomParams.size),
//                         validFormat = decimalArcsecondsValidWedge,
//                         placeholder = "0.0"
//                       )
//                     ),
//                     <.div(
//                       OffsetEditorStyles.FormRow,
//                       <.label(s"Count:"),
//                       <.label(ExploreStyles.OffsetsCount, props.pointCount.value)
//                     )
//                   )
//               },
//               gridType.get match {
//                 case GridType.Rectangular => EmptyVdom
//                 case _                    =>
//                   <.div(
//                     OffsetEditorStyles.FormRow,
//                     <.label("Refresh:"),
//                     Button(
//                       text = false,
//                       icon = Icons.ArrowsRepeat,
//                       severity = Button.Severity.Success,
//                       clazz = ExploreStyles.OffsetRegenerate
//                       // onClick = generateCurrentGrid(params, props.pointCount, updateOffsets)
//                     ).mini.compact
//                   )
//               },
//               Divider(),
//               viewOptionsSection
//             )
//           )
//         )
// }
