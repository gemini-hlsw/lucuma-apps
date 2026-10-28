// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.flamingos2

import cats.syntax.all.*
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.enums.Flamingos2WindowCover
import lucuma.core.enums.StepType
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.util.TimeSpan
import munit.FunSuite
import observe.server.CustomMasks

class Flamingos2Suite extends FunSuite:

  private val dynamic = Flamingos2DynamicConfig(
    TimeSpan.unsafeFromMicroseconds(1000000),
    none,
    Flamingos2Filter.J,
    Flamingos2ReadMode.Bright,
    Flamingos2LyotWheel.F16,
    Flamingos2FpuMask.Imaging,
    Flamingos2Decker.Imaging,
    Flamingos2ReadoutMode.Science,
    Flamingos2Reads.Reads_1,
    none
  )

  private def windowCover(
    cover:    Option[Flamingos2WindowCover],
    stepType: StepType
  ): Either[?, Flamingos2WindowCover] =
    Flamingos2
      .ccConfigFromSequenceConfig(dynamic.copy(windowCover = cover), stepType, CustomMasks.Empty)
      .map(_.windowCover)

  test("unset window cover on a dark closes the cover"):
    assertEquals(windowCover(none, StepType.Dark), Flamingos2WindowCover.Close.asRight)

  test("unset window cover on a science step opens the cover"):
    assertEquals(windowCover(none, StepType.Science), Flamingos2WindowCover.Open.asRight)

  test("explicit window cover wins over the step type"):
    assertEquals(
      windowCover(Flamingos2WindowCover.Open.some, StepType.Dark),
      Flamingos2WindowCover.Open.asRight
    )
    assertEquals(
      windowCover(Flamingos2WindowCover.Close.some, StepType.Science),
      Flamingos2WindowCover.Close.asRight
    )
