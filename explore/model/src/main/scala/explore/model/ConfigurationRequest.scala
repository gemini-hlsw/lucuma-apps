// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.refined.given
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model
import lucuma.core.model.Configuration
import lucuma.core.util.Timestamp
import lucuma.odb.json.configurationrequest.query.given
import monocle.Focus
import monocle.Lens

case class ConfigurationRequest(
  id:            ConfigurationRequest.Id,
  status:        ConfigurationRequestStatus,
  justification: Option[NonEmptyString],
  feedback:      Option[NonEmptyString],
  createdAt:     Timestamp,
  updatedAt:     Timestamp,
  configuration: Configuration
) derives Eq

object ConfigurationRequest:
  type Id = model.ConfigurationRequest.Id
  val Id = model.ConfigurationRequest.Id

  val id: Lens[ConfigurationRequest, Id]                                = Focus[ConfigurationRequest](_.id)
  val status: Lens[ConfigurationRequest, ConfigurationRequestStatus]    =
    Focus[ConfigurationRequest](_.status)
  val justification: Lens[ConfigurationRequest, Option[NonEmptyString]] =
    Focus[ConfigurationRequest](_.justification)
  val feedback: Lens[ConfigurationRequest, Option[NonEmptyString]]      =
    Focus[ConfigurationRequest](_.feedback)
  val createdAt: Lens[ConfigurationRequest, Timestamp]                  = Focus[ConfigurationRequest](_.createdAt)
  val updatedAt: Lens[ConfigurationRequest, Timestamp]                  = Focus[ConfigurationRequest](_.updatedAt)
  val configuration: Lens[ConfigurationRequest, Configuration]          =
    Focus[ConfigurationRequest](_.configuration)

  given Decoder[ConfigurationRequest] = deriveDecoder
