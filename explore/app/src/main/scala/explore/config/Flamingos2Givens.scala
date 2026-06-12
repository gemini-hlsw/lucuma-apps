// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import explore.model.display.given
import explore.utils.*
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

object Flamingos2Givens:
  given Enumerated[Option[Flamingos2ReadMode]] =
    deriveOptionalEnumerated[Flamingos2ReadMode]("Auto")
  given Display[Option[Flamingos2ReadMode]]    =
    deriveOptionalDisplay[Flamingos2ReadMode]("Auto")
