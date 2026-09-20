// sbt 2 builds of sbt-lucuma and sbt-typelevel are published here until upstream releases
resolvers += "gemini-hlsw".at(
  "https://raw.githubusercontent.com/gemini-hlsw/maven-repo/master/releases"
)

addSbtPlugin("edu.gemini"   % "sbt-lucuma-app"    % Versions.sbtLucuma)
addSbtPlugin("edu.gemini"   % "sbt-lucuma-css"    % Versions.sbtLucuma)
addSbtPlugin("edu.gemini"   % "sbt-lucuma-docker" % Versions.sbtLucuma)
// Extract metadata from sbt and make it available to the code
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo"     % Versions.sbtBuildInfo)
// Generate code for GraphQL queries
addSbtPlugin("edu.gemini"   % "sbt-clue"          % Versions.clue)
