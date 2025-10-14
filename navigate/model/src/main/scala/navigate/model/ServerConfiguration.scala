// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import lucuma.core.enums.Site

case class ServerConfiguration(
  version: String,
  site:    Site,
  odbUri:  String,
  ssoUri:  String
)
