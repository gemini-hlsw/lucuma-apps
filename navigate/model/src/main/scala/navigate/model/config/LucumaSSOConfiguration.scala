// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model.config

import cats.Eq
import cats.derived.*
import org.http4s.Uri
import org.http4s.implicits.uri

import java.security.PublicKey

/**
 * Parameeters to configure SSO
 *
 * @param serviceToken
 */
case class LucumaSSOConfiguration(
  serviceToken: String,
  publicKey:    PublicKey,
  ssoUrl:       Uri
)

object LucumaSSOConfiguration {
  val default: LucumaSSOConfiguration = LucumaSSOConfiguration(
    "",
    new PublicKey {
      override def getAlgorithm: String = ""

      override def getFormat: String = ""

      override def getEncoded: Array[Byte] = Array.empty
    },
    uri"/sso"
  )

  private given Eq[PublicKey] = Eq.fromUniversalEquals

  given Eq[LucumaSSOConfiguration] = Eq.derived[LucumaSSOConfiguration]
}
