// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import munit.FunSuite

// Already-installed PWA clients fetch the environments file at runtime and
// try to de decode it with the codec shipped in their original version.
// Changes to existing fields can make those clients fail before
// updating the service worker.
//
// These tests willl detect if the shape changed and existing clients could fail.
class AppConfigSuite extends FunSuite:

  // Ideally we'd keep a history of changes so you can decode any json shape
  // from the past. Maybe use circe-golden?
  private val DeployedJson =
    """[
      |  {
      |    "hostName": "*",
      |    "environment": "DEVELOPMENT",
      |    "odbURI": "wss://odb-dev.lucuma.xyz/ws",
      |    "odbRestURI": "https://odb-dev.lucuma.xyz",
      |    "preferencesDBURI": "wss://gpp-prefs-dev.lucuma.xyz/v1/graphql",
      |    "itcURI": "https://itc-dev.lucuma.xyz/itc",
      |    "sso": {
      |      "uri": "https://sso-dev.gpp.lucuma.xyz",
      |      "readTimeoutSeconds": 10,
      |      "expirationAnticipationSeconds": 10
      |    },
      |    "otelEndpoint": "https://otel-collector.gpp.gemini.edu/v1/traces"
      |  }
      |]""".stripMargin

  test("decodes the deployed JSON shape"):
    AppConfig.parseConf("anyhost", DeployedJson) match
      case Right(cfg) =>
        assertEquals(cfg.hostName, "*")
        assertEquals(cfg.odbURI.renderString, "wss://odb-dev.lucuma.xyz/ws")
        assertEquals(cfg.odbRestURI.renderString, "https://odb-dev.lucuma.xyz")
        assertEquals(cfg.preferencesDBURI.renderString, "wss://gpp-prefs-dev.lucuma.xyz/v1/graphql")
        assertEquals(cfg.itcURI.renderString, "https://itc-dev.lucuma.xyz/itc")
        assertEquals(
          cfg.otelEndpoint.map(_.value.renderString),
          Some("https://otel-collector.gpp.gemini.edu/v1/traces")
        )
      case Left(err)  => fail(s"Failed to decode deployed JSON: ${err.getMessage}")
