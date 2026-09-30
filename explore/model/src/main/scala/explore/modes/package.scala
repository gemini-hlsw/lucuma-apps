// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Eq
import cats.Order
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.AltairMode
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.optics.Wedge
import lucuma.core.util.NewBoolean
import lucuma.core.util.NewType
import lucuma.itc.AltairParameters

trait ModeRow:
  def instrumentConfig: ItcInstrumentConfig
  def enabled: Boolean
  def itcSupported: Boolean = enabled

object ModeWavelength extends NewType[Wavelength]
type ModeWavelength = ModeWavelength.Type

object ModeSlitSize extends NewType[Angle]:
  val milliarcseconds: Wedge[Angle, BigDecimal] =
    Angle.milliarcseconds
      .imapB(_.underlying.movePointRight(3).intValue,
             n => new java.math.BigDecimal(n).movePointLeft(3)
      )

  given Order[ModeSlitSize] = Order.by(_.value.toMicroarcseconds)

type ModeSlitSize = ModeSlitSize.Type

object ModeAO extends NewBoolean { inline def AO = True; inline def NoAO = False }
type ModeAO = ModeAO.Type

case class ScienceModes(spectroscopy: SpectroscopyModesMatrix, imaging: ImagingModesMatrix)
    derives Eq

object ScienceModes:
  val empty = ScienceModes(SpectroscopyModesMatrix.empty, ImagingModesMatrix.empty)

object AltairModeRows:
  // LGS+P1 performance does not depend on the guide star, so it is left to the guider selector
  // once the mode exists instead of getting a row of its own.
  val TableAltairModes: List[AltairMode] = List(AltairMode.Ngs, AltairMode.Lgs)

  // Each row of an instrument supporting Altair is followed by one copy per table Altair mode.
  def expand[A](rows: List[A])(instrumentConfig: A => ItcInstrumentConfig)(
    withAltair: (A, AltairMode) => A
  ): List[A] =
    rows.flatMap: row =>
      if instrumentConfig(row).instrument.supportsAltair then
        row :: TableAltairModes.map(withAltair(row, _))
      else List(row)

  // A row's Altair mode is only usable with the parameters of a guide star for it.
  def instrumentConfigWith(
    instrumentConfig: ItcInstrumentConfig,
    altair:           Option[AltairMode],
    parameters:       Map[AltairMode, AltairParameters]
  ): Option[ItcInstrumentConfig] =
    altair.fold(instrumentConfig.some): mode =>
      parameters.get(mode).map(p => instrumentConfig.withAltair(p.some))

  def expandSpectroscopy(rows: List[SpectroscopyModeRow]): List[SpectroscopyModeRow] =
    expand(rows)(_.instrumentConfig)((row, mode) => row.copy(altair = mode.some))

  def expandImaging(rows: List[ImagingModeRow]): List[ImagingModeRow] =
    expand(rows)(_.instrumentConfig)((row, mode) => row.copy(altair = mode.some))
