// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
      |    "odbURI": "wss://odb-dev.gpp.gemini.edu/ws",
      |    "odbRestURI": "https://odb-dev.gpp.gemini.edu",
      |    "preferencesDBURI": "wss://prefs-dev.gpp.gemini.edu/v1/graphql",
      |    "itcURI": "https://itc-dev.gpp.gemini.edu/itc",
      |    "sso": {
      |      "uri": "https://sso-dev.gpp.gemini.edu",
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
        assertEquals(cfg.odbURI.renderString, "wss://odb-dev.gpp.gemini.edu/ws")
        assertEquals(cfg.odbRestURI.renderString, "https://odb-dev.gpp.gemini.edu")
        assertEquals(cfg.preferencesDBURI.renderString, "wss://prefs-dev.gpp.gemini.edu/v1/graphql")
        assertEquals(cfg.itcURI.renderString, "https://itc-dev.gpp.gemini.edu/itc")
        assertEquals(
          cfg.otelEndpoint.map(_.value.renderString),
          Some("https://otel-collector.gpp.gemini.edu/v1/traces")
        )
      case Left(err)  => fail(s"Failed to decode deployed JSON: ${err.getMessage}")
