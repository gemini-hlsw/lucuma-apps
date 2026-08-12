// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.syntax.all.*
import lucuma.core.enums.GnirsDisperserOrder
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

object GnirsCrossDispersed:

  private val HBandOrder: Int = 4

  // Enumerated order matters where two orders share a boundary wavelength: order 3 is
  // declared before order 4, so 1.86µm resolves to order 3, as it does in the OCS.
  private val CrossDispersedOrders: List[GnirsDisperserOrder] =
    Enumerated[GnirsDisperserOrder].all.filter(_.crossDispersed)

  /**
   * The H-band (order 4) equivalent of a wavelength observed in another order, following
   * λ₂ = λ₁ · m₁ / m₂.  For example 2.20µm, which falls in order 3, becomes
   * 2.20µm · 3/4 = 1.65µm.
   *
   * A cross-dispersed setting covers several orders at once, so it is specified by its
   * H-band wavelength: that makes the acquisition default to the H filter and sends an
   * H-band wavelength to the TCS, minimizing differential atmospheric refraction.
   *
   * The wavelength is returned unchanged when it falls outside every cross-dispersed order.
   */
  def toHBandWavelength(wavelength: Wavelength): Wavelength =
    CrossDispersedOrders
      .find(o => o.minWavelength <= wavelength && wavelength <= o.maxWavelength)
      .flatMap: order =>
        val pm = wavelength.toPicometers.value.value.toLong * order.count
        Wavelength.fromIntPicometers(((pm + HBandOrder / 2) / HBandOrder).toInt)
      .getOrElse(wavelength)
