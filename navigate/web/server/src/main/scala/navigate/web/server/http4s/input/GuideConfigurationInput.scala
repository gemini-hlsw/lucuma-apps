// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s.input

import cats.syntax.all.*
import grackle.syntax.*
import lucuma.core.enums.ComaOption
import lucuma.core.enums.MountGuideOption
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig
import lucuma.core.model.ProbeGuide
import lucuma.core.model.TelescopeGuideConfig
import lucuma.odb.graphql.binding.*

object ProbeGuideInput:

  /** An empty probe guide input means that no probe guide is requested. */
  val Binding: Matcher[Option[ProbeGuide]] =
    ObjectFieldsBinding.rmap:
      case List(
            GuideProbeBinding.Option("from", rFrom),
            GuideProbeBinding.Option("to", rTo)
          ) =>
        (rFrom, rTo).parTupled.flatMap:
          case (Some(from), Some(to)) => ProbeGuide(from, to).some.success
          case (None, None)           => none[ProbeGuide].success
          case _                      => Matcher.validationFailure("Both from and to must be specified.")

object GuideConfigurationInput:

  val Binding: Matcher[TelescopeGuideConfig] =
    ObjectFieldsBinding.rmap:
      case List(
            TipTiltSourceBinding.List.Option("m2Inputs", rM2Inputs),
            BooleanBinding.Option("m2Coma", rM2Coma),
            M1SourceBinding.Option("m1Input", rM1Input),
            BooleanBinding("mountOffload", rMountOffload),
            BooleanBinding("daytimeMode", rDaytimeMode),
            ProbeGuideInput.Binding.Option("probeGuide", rProbeGuide)
          ) =>
        (rM2Inputs, rM2Coma, rM1Input, rMountOffload, rDaytimeMode, rProbeGuide).parMapN:
          (m2Inputs, m2Coma, m1Input, mountOffload, daytimeMode, probeGuide) =>
            val m2 = m2Inputs.orEmpty
            TelescopeGuideConfig(
              MountGuideOption(mountOffload),
              m1Input.fold(M1GuideConfig.M1GuideOff)(M1GuideConfig.M1GuideOn(_)),
              if m2.isEmpty then M2GuideConfig.M2GuideOff
              else
                M2GuideConfig.M2GuideOn(
                  ComaOption(m2Coma.contains(true) && m1Input.isDefined),
                  m2.toSet
                )
              ,
              daytimeMode.some,
              probeGuide.flatten
            )
