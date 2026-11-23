// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Eq
import cats.syntax.all.*
import crystal.react.View
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.config.DaytimeCalibrationSetsTable.*
import explore.model.Attachment
import explore.model.AttachmentList
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Defined
import lucuma.core.model.MaskDefinition
import lucuma.core.model.ToBeDefined
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.DropdownOptional
import lucuma.react.primereact.SelectItem
import lucuma.ui.sequence.SequenceStyles

/**
 * The FPU of a daytime calibration set: a builtin FPU or a MOS mask of the program. Masks follow a
 * "Masks" header, as the dropdown binding has no option groups.
 */
private[config] object DaytimeFpuColumn:

  /** The masks a set may pick, and the attachments, to name a current mask that is not offered. */
  case class Masks(offered: List[Attachment], attachments: AttachmentList)

  object Masks:
    given Eq[Masks] = Eq.by(m => (m.offered, m.attachments.toList))

  private enum Item[+U]:
    case Builtin(fpu: U)
    case Mask(mask: MaskDefinition)
    case Header

  private given [U]: Eq[Item[U]] = Eq.fromUniversalEquals

  private def toItem[U](v: Either[U, MaskDefinition]): Item[U] =
    v.fold(Item.Builtin(_), Item.Mask(_))

  /**
   * `masks` is defined when masks are offered. A builtin in `pinholes` is offered to staff only,
   * unless it is the current value. `width` is the slit width of a custom mask, for the tooltip.
   */
  def apply[U: Enumerated: Display](
    editable:    Boolean,
    isStaff:     Boolean,
    masks:       Option[Masks],
    pinholes:    Set[U],
    placeholder: Row => String,
    width:       Row => Option[String]
  )(view: Int => View[Option[Either[U, MaskDefinition]]]): ColDef.Type =
    cellColumn(FpuColumnId, "FPU", 150, withOptics): r =>
      val value   = view(r._2)
      val current = value.get
      val loaded  = masks.foldMap(_.offered)

      def maskLabel(m: MaskDefinition): String =
        m match
          case ToBeDefined => "Mask missing"
          case Defined(id) =>
            masks
              .flatMap(_.attachments.get(id))
              .fold(s"Mask $id")(_.displayName.value)

      val excluded: Set[U] = pinholes.excludedUnless(isStaff, current.flatMap(_.left.toOption))

      val builtins: List[SelectItem[Item[U]]] =
        Enumerated[U].all
          .filterNot(excluded.contains)
          .map(u => SelectItem(Item.Builtin(u), Display[U].shortName(u)))

      // The current mask stays shown when it is not offered, e.g. deleted or of another program.
      val currentMask: List[SelectItem[Item[U]]] =
        current
          .flatMap(_.toOption)
          .filterNot:
            case Defined(id) => loaded.exists(_.id === id)
            case ToBeDefined => false
          .map(m => SelectItem[Item[U]](Item.Mask(m), maskLabel(m), disabled = true))
          .toList

      val maskItems: List[SelectItem[Item[U]]] =
        currentMask ++ loaded.map(m => SelectItem(Item.Mask(Defined(m.id)), m.displayName.value))

      val options: List[SelectItem[Item[U]]] =
        builtins ++
          (if maskItems.isEmpty then Nil
           else SelectItem[Item[U]](Item.Header, "Masks", disabled = true) :: maskItems)

      val dropdown: VdomNode =
        DropdownOptional[Item[U]](
          id = cellId(FpuColumnId, r._2).value,
          value = current.map(toItem),
          options = options,
          placeholder = placeholder(r),
          showClear = true,
          filter = masks.isDefined,
          clazz = SequenceStyles.SequenceDropdown,
          panelClass = SequenceStyles.SequenceDropdownPanel,
          disabled = !editable,
          onChange = _.fold(value.set(none)):
            case Item.Builtin(u) => value.set(u.asLeft.some)
            case Item.Mask(m)    => value.set(m.asRight.some)
            case Item.Header     => Callback.empty
        )

      val missing: Boolean = current.contains(ToBeDefined.asRight)

      val cell =
        <.div(ExploreStyles.DaytimeCalFpu)(
          dropdown,
          <.span(Icons.ExclamationTriangle.withClass(ExploreStyles.WarningIcon))
            .withTooltip("The mask was deleted, choose another one")
            .when(missing)
        )

      width(r).fold(cell)(w => cell.withTooltip(s"Slit width $w"))
