// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.input

import lucuma.ui.input.facade.Inputmask

/**
 * Coherence laws between the two halves of a [[InputCodec]]. They run Inputmask itself, so the
 * test environment needs a DOM.
 */
final case class InputCodecLaws[A](format: InputCodec[A]):

  /**
   * The canonical text of every value is valid for the mask. On blur the component writes it into
   * the input and Inputmask re-masks it, so a mismatch would mangle the displayed value.
   */
  def canonicalIsValidForMask(a: A): Boolean =
    format.mask.forall(options => Inputmask.isValid(format.valid.reverseGet(a), options))

  /**
   * The canonical text parses back to a value that prints the same, so redisplaying a value never
   * shows an error.
   */
  def canonicalRoundTrips(a: A): Boolean =
    val canonical: String = format.valid.reverseGet(a)
    format.valid.getValid(canonical).toOption.map(format.valid.reverseGet).contains(canonical)
