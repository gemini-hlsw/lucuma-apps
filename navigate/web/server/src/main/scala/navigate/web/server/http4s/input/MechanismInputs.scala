// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s.input

import cats.syntax.all.*
import grackle.syntax.*
import lucuma.core.util.Enumerated
import lucuma.odb.graphql.binding.*
import navigate.model.AcWindow
import navigate.model.enums.ShutterMode

object AcWindowInput:

  private enum Size(val tag: String) derives Enumerated:
    case Full          extends Size("full")
    case Window200x200 extends Size("window_200x200")
    case Window100x100 extends Size("window_100x100")

  private val SizeBinding: Matcher[Size] = enumeratedBinding

  val CenterBinding: Matcher[(Int, Int)] =
    ObjectFieldsBinding.rmap:
      case List(
            IntBinding("x", rX),
            IntBinding("y", rY)
          ) =>
        (rX, rY).parTupled

  val Binding: Matcher[AcWindow] =
    ObjectFieldsBinding.rmap:
      case List(
            SizeBinding("type", rType),
            CenterBinding.Option("center", rCenter)
          ) =>
        (rType, rCenter).parFlatMapN:
          case (Size.Full, _)                     => AcWindow.Full.success
          case (Size.Window200x200, Some((x, y))) => AcWindow.Square200(x, y).success
          case (Size.Window100x100, Some((x, y))) => AcWindow.Square100(x, y).success
          case (t, None)                          =>
            Matcher.validationFailure(
              s"A center must be specified for window size ${t.tag.toUpperCase}."
            )

object ShutterModeInput:

  private enum Mode(val tag: String) derives Enumerated:
    case FullyOpen extends Mode("fully_open")
    case Tracking  extends Mode("tracking")

  private val ModeBinding: Matcher[Mode] = enumeratedBinding

  val Binding: Matcher[ShutterMode] =
    ObjectFieldsBinding.rmap:
      case List(
            ModeBinding("mode", rMode),
            DistanceInput.Binding.Option("aperture", rAperture)
          ) =>
        (rMode, rAperture).parFlatMapN:
          case (Mode.FullyOpen, _)      => ShutterMode.FullyOpen.success
          case (Mode.Tracking, Some(a)) => ShutterMode.Tracking(a).success
          case (Mode.Tracking, None)    =>
            Matcher.validationFailure("An aperture must be specified for TRACKING mode.")
