// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import lucuma.react.common.Css
import lucuma.react.fa.*

import scala.scalajs.js
import scala.scalajs.js.annotation.*

object SequenceIcons:
  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faCheck")
  private val faCheck: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-light-svg-icons", "faChevronDown")
  private val faChevronDown: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faCircle")
  private val faCircle: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-light-svg-icons", "faClone")
  private val faClone: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faCrosshairs")
  private val faCrosshairs: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faGripDotsVertical")
  private val faGripDotsVertical: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faSquare")
  private val faSquare: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-light-svg-icons", "faTrashCan")
  private val faTrash: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/sharp-solid-svg-icons", "faXmark")
  private val faXMark: FAIcon = js.native

  // This is tedious but lets us do proper tree-shaking
  FontAwesome.library.add(
    faCheck,
    faChevronDown,
    faCircle,
    faClone,
    faCrosshairs,
    faSquare,
    faTrash,
    faXMark
  )

  // TODO Color
  private def letterLayeredIcon(icon: FontAwesomeIcon, letter: Char, clazz: Css): LayeredIcon =
    LayeredIcon(clazz = clazz, fixedWidth = true)(
      icon,
      TextLayer(letter.toString).withInverse().withSize(IconSize.SM)
    )

  inline def Check            = FontAwesomeIcon(faCheck)
  inline def ChevronDown      = FontAwesomeIcon(faChevronDown)
  inline def Circle           = FontAwesomeIcon(faCircle)
  inline def Clone            = FontAwesomeIcon(faClone)
  inline def Crosshairs       = FontAwesomeIcon(faCrosshairs)
  inline def GripDotsVertical = FontAwesomeIcon(faGripDotsVertical)
  inline def Square           = FontAwesomeIcon(faSquare)
  inline def Trash            = FontAwesomeIcon(faTrash)
  inline def XMark            = FontAwesomeIcon(faXMark)

  object StepType:
    val Bias   = letterLayeredIcon(Square, 'B', SequenceStyles.StepType.Bias)
    val Dark   = letterLayeredIcon(Square, 'D', SequenceStyles.StepType.Dark)
    val Arc    = letterLayeredIcon(Square, 'A', SequenceStyles.StepType.Arc)
    val Flat   = letterLayeredIcon(Square, 'F', SequenceStyles.StepType.Flat)
    val Object = letterLayeredIcon(Circle, 'O', SequenceStyles.StepType.Object)
