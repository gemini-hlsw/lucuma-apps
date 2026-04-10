// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.arb

import eu.timepit.refined.scalacheck.string.given
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.enums.Site
import lucuma.core.math.arb.ArbRefined.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbNewType.given
import observe.model.ClientConfig
import observe.model.ClientId
import observe.model.SubsystemOrServer
import observe.model.Version
import observe.model.arb.ArbSubsystem.given
import observe.model.enums.ControlStrategy
import org.http4s.Uri
import org.http4s.laws.discipline.arbitrary.http4sTestingCogenForUri
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbClientConfig:
  // Checks that a URI is converting to String and back to URI safely.
  // Also, ensures that the URI has no fragment: the codec struggles with those.
  // Retries until it finds a valid URI with these conditions, so that property testing doesn't give up.
  given Arbitrary[Uri] = Arbitrary(
    org.http4s.laws.discipline.arbitrary.http4sTestingArbitraryForUri.arbitrary
      .map(uri => Uri.fromString(uri.toString).map(_.withoutFragment))
      .retryUntil(_.isRight)
      .map(_.toOption.get)
  )

  given Arbitrary[ClientConfig] = Arbitrary:
    for
      site                       <- arbitrary[Site]
      environment                <- arbitrary[ExecutionEnvironment]
      odbUri                     <- arbitrary[Uri]
      ssoUri                     <- arbitrary[Uri]
      exploreBaseUri             <- arbitrary[Uri]
      clientId                   <- arbitrary[ClientId]
      version                    <- arbitrary[Version]
      subsystemControlStrategies <- arbitrary[Map[SubsystemOrServer, ControlStrategy]]
    yield ClientConfig(
      site,
      environment,
      odbUri,
      ssoUri,
      exploreBaseUri,
      clientId,
      version,
      subsystemControlStrategies
    )

  given Cogen[ClientConfig] =
    Cogen[
      (Site,
       ExecutionEnvironment,
       Uri,
       Uri,
       Uri,
       ClientId,
       Version,
       List[(SubsystemOrServer, ControlStrategy)]
      )
    ].contramap(x =>
      (x.site,
       x.environment,
       x.odbUri,
       x.ssoUri,
       x.exploreBaseUri,
       x.clientId,
       x.version,
       x.subsystemControlStrategies.toList
      )
    )

object ArbClientConfig extends ArbClientConfig
