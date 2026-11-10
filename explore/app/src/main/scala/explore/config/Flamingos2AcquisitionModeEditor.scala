// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.*
import explore.config.offsets.OffsetInput
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Flamingos2AcquisitionType
import lucuma.core.math.Offset
import lucuma.core.model.sequence.flamingos2.Flamingos2AcquisitionMode
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

/**
 * The acquisition type and, for a Faint acquisition, its sky offset. With no `defaultMode` the type
 * is chosen by the ODB unless overridden, which is offered as "Auto". With one, that mode is what
 * the ODB uses when none is explicit, so it is shown and edited as the starting value.
 */
final case class Flamingos2AcquisitionModeEditor(
  mode:                     View[Option[Flamingos2AcquisitionMode]],
  defaultMode:              Option[Flamingos2AcquisitionMode],
  idPrefix:                 NonEmptyString,
  disabled:                 Boolean,
  showCustomization:        Boolean,
  allowRevertCustomization: Boolean
) extends ReactFnProps(Flamingos2AcquisitionModeEditor)

object Flamingos2AcquisitionModeEditor
    extends ReactFnComponent[Flamingos2AcquisitionModeEditor](props =>
      given Enumerated[Option[Flamingos2AcquisitionType]] =
        deriveOptionalEnumerated[Flamingos2AcquisitionType]("Auto")
      given Display[Option[Flamingos2AcquisitionType]]    =
        deriveOptionalDisplay[Flamingos2AcquisitionType]("Auto")

      // Edits apply to the default mode when none is stored, so that changing just the sky
      // offset of the default takes effect. Arriving back at the default stores none.
      val effectiveMode: View[Option[Flamingos2AcquisitionMode]] =
        props.defaultMode.fold(props.mode): default =>
          props.mode.zoom(_.orElse(default.some))(mod =>
            stored => mod(stored.orElse(default.some)).filterNot(_ === default)
          )

      val typeView: View[Option[Flamingos2AcquisitionType]] =
        effectiveMode.zoom(_.map(_.acquisitionType))(mod =>
          mode =>
            mod(mode.map(_.acquisitionType)).map:
              Flamingos2AcquisitionMode
                .forTypeAndOffset(_, Flamingos2AcquisitionMode.Faint.DefaultSkyOffset)
        )

      val skyOffsetViewOpt: Option[View[Offset]] =
        effectiveMode.toOptionView
          .flatMap(_.zoom(Flamingos2AcquisitionMode.skyOffset).toOptionView)

      val typeId: NonEmptyString   = NonEmptyString.unsafeFrom(s"${props.idPrefix.value}-type")
      val offsetId: NonEmptyString = NonEmptyString.unsafeFrom(s"${props.idPrefix.value}-offset")

      val typeSelect: VdomNode =
        props.defaultMode.fold[VdomNode](
          CustomizableEnumSelect(
            id = typeId,
            view = typeView,
            defaultValue = none,
            label = "Type".some,
            helpId = Some("configuration/f2/acquisition-type.md".refined),
            disabled = props.disabled,
            showCustomization = props.showCustomization,
            allowRevertCustomization = props.allowRevertCustomization
          )
        )(default =>
          CustomizableEnumSelectOptional(
            id = typeId,
            view = typeView,
            defaultValue = default.acquisitionType.some,
            label = "Type".some,
            helpId = Some("configuration/f2/acquisition-type.md".refined),
            disabled = props.disabled,
            showCustomization = props.showCustomization,
            allowRevertCustomization = props.allowRevertCustomization
          )
        )

      React.Fragment(
        typeSelect,
        skyOffsetViewOpt.map: skyOffsetView =>
          OffsetInput(
            id = offsetId,
            offset = skyOffsetView,
            readonly = props.disabled,
            clazz = LucumaPrimeStyles.FormField
          )
      )
    )
