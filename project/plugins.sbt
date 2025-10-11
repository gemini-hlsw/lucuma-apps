addSbtPlugin("edu.gemini"   % "sbt-lucuma-app"    % Versions.sbtLucuma)
addSbtPlugin("edu.gemini"   % "sbt-lucuma-css"    % Versions.sbtLucuma)
addSbtPlugin("edu.gemini"   % "sbt-lucuma-docker" % Versions.sbtLucuma)
// Extract metadata from sbt and make it available to the code
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo"     % Versions.sbtBuildInfo)
// Generate code for GraphQL queries
addSbtPlugin("edu.gemini"   % "sbt-clue"          % Versions.clue)
// sbt revolver makes it easier to launch applications from the sbt console
addSbtPlugin("io.spray"     % "sbt-revolver"      % Versions.sbtRevolver)
