// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import lucuma.core.geom.OffsetGenerator
import lucuma.core.model.sequence.TelescopeConfig
import monocle.Focus
import monocle.Iso
import monocle.Prism
import monocle.macros.GenPrism

enum TelescopeConfigGenerator derives Eq:
  case Enumerated(values: NonEmptyList[TelescopeConfig])     extends TelescopeConfigGenerator
  case FromOffsetGenerator(offsetGenerator: OffsetGenerator) extends TelescopeConfigGenerator

object TelescopeConfigGenerator:
  val enumerated: Prism[TelescopeConfigGenerator, Enumerated]                   =
    GenPrism[TelescopeConfigGenerator, Enumerated]
  val fromOffsetGenerator: Prism[TelescopeConfigGenerator, FromOffsetGenerator] =
    GenPrism[TelescopeConfigGenerator, FromOffsetGenerator]

  object Enumerated:
    val values: Iso[TelescopeConfigGenerator.Enumerated, NonEmptyList[TelescopeConfig]] =
      Focus[TelescopeConfigGenerator.Enumerated](_.values)

  object FromOffsetGenerator:
    val offsetGenerator: Iso[TelescopeConfigGenerator.FromOffsetGenerator, OffsetGenerator] =
      Focus[TelescopeConfigGenerator.FromOffsetGenerator](_.offsetGenerator)
