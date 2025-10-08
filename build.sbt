import _root_.cats.effect.kernel.syntax.resource
import Dependencies.*
import Versions.*
import sbt.Keys.*
import NativePackagerHelper.*
import org.scalajs.linker.interface.ModuleSplitStyle
import scala.sys.process.*
import sbt.nio.file.FileTreeView

// Build JS module for deployment, only used for observe web client
val buildJsModule = taskKey[File]("Build module for deployment")

name := "lucuma-apps"

// TODO REMOVE ONCE THIS WORKS AGAIN
ThisBuild / tlCiScalafmtCheck := false
ThisBuild / tlCiScalafixCheck := false

ThisBuild / resolvers := List(Resolver.mavenLocal)

val pushCond          = "github.event_name == 'push'"
val prCond            = "github.event_name == 'pull_request'"
val mainCond          = "github.ref == 'refs/heads/main'"
val notMainCond       = "github.ref != 'refs/heads/main'"
val geminiRepoCond    = "startsWith(github.repository, 'gemini')"
val notDependabotCond = "github.actor != 'dependabot[bot]'"
val isMergedCond      = "github.event.pull_request.merged == true"
def allConds(conds: String*) = conds.mkString("(", " && ", ")")
def anyConds(conds: String*) = conds.mkString("(", " || ", ")")

val faNpmAuthToken = "FONTAWESOME_NPM_AUTH_TOKEN" -> "${{ secrets.FONTAWESOME_NPM_AUTH_TOKEN }}"
val herokuToken    = "HEROKU_API_KEY"             -> "${{ secrets.HEROKU_API_KEY }}"

ThisBuild / githubWorkflowSbtCommand := "sbt -v -J-Xmx6g"
ThisBuild / githubWorkflowEnv += faNpmAuthToken
ThisBuild / githubWorkflowEnv += herokuToken

lazy val setupNodeNpmInstall =
  List(
    WorkflowStep.Use(
      UseRef.Public("actions", "setup-node", "v4"),
      name = Some("Setup Node.js"),
      params = Map(
        "node-version"          -> "20",
        "cache"                 -> "npm",
        "cache-dependency-path" -> "modules/web/client/package-lock.json"
      )
    ),
    WorkflowStep.Use(
      UseRef.Public("actions", "cache", "v3"),
      name = Some("Cache node_modules"),
      id = Some("cache-node_modules"),
      params = {
        val prefix = "node_modules"
        val key    = s"$prefix-$${{ hashFiles('modules/web/client/package-lock.json') }}"
        Map("path" -> "node_modules", "key" -> key, "restore-keys" -> prefix)
      }
    ),
    WorkflowStep.Run(
      List("cd modules/web/client", "npm clean-install --verbose"),
      name = Some("npm clean-install"),
      cond = Some("steps.cache-node_modules.outputs.cache-hit != 'true'")
    )
  )

lazy val dockerHubLogin =
  WorkflowStep.Run(
    List(
      "echo ${{ secrets.DOCKER_HUB_TOKEN }} | docker login --username nlsoftware --password-stdin"
    ),
    name = Some("Login to Docker Hub")
  )

lazy val sbtDockerPublish =
  WorkflowStep.Sbt(
    List("clean", "deploy/docker:publish"),
    name = Some("Build and Publish Docker image")
  )

lazy val herokuRelease =
  WorkflowStep.Run(
    List(
      "npm install -g heroku",
      "heroku container:login",
      "docker tag noirlab/gpp-obs registry.heroku.com/${{ vars.HEROKU_APP_NAME_GN || 'observe-dev-gn' }}/web",
      "docker push registry.heroku.com/${{ vars.HEROKU_APP_NAME_GN || 'observe-dev-gn' }}/web",
      "heroku container:release web -a ${{ vars.HEROKU_APP_NAME_GN || 'observe-dev-gn' }} -v",
      "docker tag noirlab/gpp-obs registry.heroku.com/${{ vars.HEROKU_APP_NAME_GS || 'observe-dev-gs' }}/web",
      "docker push registry.heroku.com/${{ vars.HEROKU_APP_NAME_GS || 'observe-dev-gs' }}/web",
      "heroku container:release web -a ${{ vars.HEROKU_APP_NAME_GS || 'observe-dev-gs' }} -v"
    ),
    name = Some("Deploy and release app in Heroku")
  )

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    "deploy",
    "Build and publish Docker image / Deploy to Heroku",
    githubWorkflowJobSetup.value.toList :::
      setupNodeNpmInstall :::
      dockerHubLogin ::
      sbtDockerPublish ::
      herokuRelease ::
      Nil,
    scalas = List(scalaVersion.value),
    javas = githubWorkflowJavaVersions.value.toList.take(1),
    cond = Some(allConds(mainCond, geminiRepoCond))
  )

ThisBuild / lucumaCssExts += "svg"

Global / onChangedBuildSource                   := ReloadOnSourceChanges
ThisBuild / scalafixDependencies += "edu.gemini" % "lucuma-schemas_3" % lucumaUiSchemas
ThisBuild / scalaVersion                        := "3.7.3"
ThisBuild / crossScalaVersions                  := Seq("3.7.3")
ThisBuild / scalacOptions ++= Seq(
  "-language:implicitConversions",
  // ScalablyTyped macros introduce deprecated methods, this silences those warnings
  "-Wconf:msg=linkingInfo in package scala.scalajs.runtime is deprecated:s"
)
ThisBuild / scalafixResolvers += coursierapi.MavenRepository.of(
  "https://s01.oss.sonatype.org/content/repositories/snapshots/"
)

// Gemini repository
ThisBuild / resolvers += "Gemini Repository".at(
  "https://github.com/gemini-hlsw/maven-repo/raw/master/releases"
)

// TODO Remove once we stop using http4s snapshot
ThisBuild / resolvers += "s01-oss-sonatype-org-snapshots".at(
  "https://s01.oss.sonatype.org/content/repositories/snapshots"
)

ThisBuild / evictionErrorLevel := Level.Info

// Uncomment for local gmp testing
// ThisBuild / resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

enablePlugins(GitBranchPrompt)

lazy val esModule = Seq(
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
  Compile / fastLinkJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
  Compile / fullLinkJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
  Compile / fastLinkJS / scalaJSLinkerConfig ~= (_.withModuleSplitStyle(
    ModuleSplitStyle.FewestModules
  )),
  Compile / fullLinkJS / scalaJSLinkerConfig ~= (_.withModuleSplitStyle(
    ModuleSplitStyle.FewestModules
  ))
)

//////////////
// Projects
//////////////

lazy val root = tlCrossRootProject.aggregate(
  explore_model,
  explore_modelTests,
  explore_common,
  explore_app,
  explore_workers,
  observe_web_server,
  observe_web_client,
  observe_server,
  observe_model,
  observe_ui_model
)

// START EXPLORE

lazy val exploreCommonSettings = lucumaGlobalSettings ++ Seq(
  scalacOptions ~= (_.filterNot(Set("-Vtype-diffs")))
)

lazy val exploreCommonLibSettings = Seq(
  libraryDependencies ++=
    Cats.value ++
      CatsEffect.value ++
      CatsRetry.value ++
      Circe.value ++
      Clue.value ++
      Crystal.value ++
      Fs2.value ++
      Http4sCore.value ++
      Kittens.value ++
      LucumaCore.value ++
      LucumaSchemas.value ++
      LucumaOdbSchema.value ++
      LucumaAgs.value ++
      LucumaITCClient.value ++
      Monocle.value ++
      Mouse.value ++
      Boopickle.value ++
      In(Test)(
        MUnit.value ++
          MUnitScalaCheck.value ++
          Discipline.value ++
          CatsTimeTestkit.value ++
          CatsEffectTestkit.value ++
          MUnitCatsEffect.value ++
          MonocleLaw.value
      ),
  // temporary? fix for upgrading to Scala 3.7
  libraryDependencies += "org.scala-lang" %% "scala3-library" % scalaVersion.value,
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val exploreTestkitLibSettings = Seq(
  libraryDependencies ++= Discipline.value ++
    MonocleLaw.value ++
    CatsTimeTestkit.value ++
    CatsEffectTestkit.value ++
    LucumaCoreTestkit.value ++
    LucumaCatalogTestkit.value ++
    LucumaSchemasTestkit.value
)

lazy val exploreCommonJvmSettings = Seq(
  libraryDependencies ++=
    Fs2Io.value
)

lazy val exploreCommonJsLibSettings =
  exploreCommonLibSettings ++ Seq(
    libraryDependencies ++=
      ClueScalaJs.value ++
        Http4sDom.value ++
        Fs2Dom.value ++
        Log4Cats.value ++
        Log4CatsLogLevel.value ++
        ScalaCollectionContrib.value ++
        ScalaJsReact.value ++
        ScalaJSDom.value ++
        LucumaUI.value ++
        In(Test)(ScalaJsReactTest.value),
    dependencyOverrides ++= ScalaJsReact.value
  )

lazy val exploreCommonModuleTest = Seq(
  Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
)

lazy val exploreEsModule = Seq(
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
  Compile / fastLinkJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
  Compile / fullLinkJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
  Compile / fullLinkJS / scalaJSLinkerConfig ~= { _.withMinify(true) },
  Compile / fastLinkJS / scalaJSLinkerConfig ~= (_.withModuleSplitStyle(
    // Linking with smaller modules seems to take way longer.
    // ModuleSplitStyle.SmallModulesFor(List("explore"))
    ModuleSplitStyle.FewestModules
  )),
  Compile / fullLinkJS / scalaJSLinkerConfig ~= (_.withModuleSplitStyle(
    ModuleSplitStyle.FewestModules
  ))
)

lazy val explore_model = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("explore/model"))
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonLibSettings: _*)
  .jvmSettings(exploreCommonJvmSettings)
  .jsSettings(exploreCommonJsLibSettings)

lazy val explore_modelTestkit = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("explore/model-testkit"))
  .dependsOn(explore_model)
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonLibSettings: _*)
  .settings(exploreTestkitLibSettings: _*)
  .jsSettings(exploreCommonModuleTest: _*)
  .jvmSettings(exploreCommonJvmSettings)

lazy val explore_modelTests = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("explore_model-tests"))
  .dependsOn(explore_modelTestkit)
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonLibSettings: _*)
  .jsSettings(exploreCommonModuleTest: _*)
  .jvmSettings(exploreCommonJvmSettings)

lazy val explore_workers = project
  .in(file("explore/workers"))
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonJsLibSettings: _*)
  .settings(exploreCommonLibSettings: _*)
  .settings(exploreEsModule: _*)
  .settings(
    libraryDependencies ++= LucumaCatalog.value ++
      Http4sDom.value ++
      Log4Cats.value,
    Test / scalaJSLinkerConfig ~= {
      import org.scalajs.linker.interface.OutputPatterns
      _.withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))
    }
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(explore_model.js)

lazy val explore_common = project
  .in(file("explore/common"))
  .dependsOn(explore_model.js, explore_modelTestkit.js % Test)
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonJsLibSettings: _*)
  .settings(exploreCommonModuleTest: _*)
  .settings(
    libraryDependencies ++=
      LucumaSsoFrontendClient.value ++
        LucumaCatalog.value ++
        LucumaSchemas.value ++
        LucumaReact.value ++
        In(Test)(LucumaUITestkit.value),
    buildInfoKeys    := Seq[BuildInfoKey](
      scalaVersion,
      sbtVersion,
      git.gitHeadCommit,
      "buildDateTime" -> System.currentTimeMillis()
    ),
    buildInfoPackage := "explore"
  )
  .enablePlugins(ScalaJSPlugin, BuildInfoPlugin)

lazy val explore_app: Project = project
  .in(file("explore/app"))
  .dependsOn(explore_model.js, explore_common)
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonJsLibSettings: _*)
  .settings(exploreEsModule: _*)
  .enablePlugins(ScalaJSPlugin, LucumaCssPlugin, CluePlugin)
  .settings(
    Test / test          := {},
    coverageEnabled      := false,
    libraryDependencies ++=
      GeminiLocales.value ++
        LucumaReact.value,
    // Build workers when you build explore
    Compile / fastLinkJS := (Compile / fastLinkJS)
      .dependsOn(explore_workers / Compile / fastLinkJS)
      .value,
    Compile / fullLinkJS := (Compile / fullLinkJS)
      .dependsOn(explore_workers / Compile / fullLinkJS)
      .value
  )

// START OBSERVE

lazy val observeCommonSettings = Seq(
  Compile / packageDoc / mappings := Seq(),
  Compile / doc / sources         := Seq.empty,
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val stateengine = project
  .in(file("modules/stateengine"))
  .settings(
    name := "stateengine",
    libraryDependencies ++=
      Mouse.value ++
        Fs2.value ++
        MUnit.value ++
        Cats.value ++
        CatsEffect.value
  )

lazy val observe_web_server = project
  .in(file("modules/web/server"))
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(GitBranchPrompt)
  .settings(observeCommonSettings: _*)
  .settings(
    libraryDependencies ++=
      UnboundId.value ++
        LucumaSsoBackendClient.value ++
        JwtCore.value ++
        JwtCirce.value ++
        Http4sServer.value ++
        Log4CatsNoop.value ++
        Http4sJdkClient.value ++
        Http4sServer.value ++
        PureConfig.value ++
        Logback.value ++
        JuliSlf4j.value,
    // Supports launching the server in the background
    reStart / mainClass := Some("observe.web.server.http4s.WebServerLauncher")
  )
  .settings(
    buildInfoUsePackageAsPath := true,
    buildInfoKeys ++= Seq[BuildInfoKey](name, version, buildInfoBuildNumber),
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoObject           := "OcsBuildInfo",
    buildInfoPackage          := "observe.web.server"
  )
  .dependsOn(observe_server)
  .dependsOn(observe_model.jvm % "compile->compile;test->test")

lazy val observe_ui_model = project
  .in(file("modules/web/client-model"))
  .settings(lucumaGlobalSettings: _*)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    coverageEnabled := false,
    libraryDependencies ++=
      Crystal.value ++
        LucumaUI.value ++
        LucumaSchemas.value ++
        LucumaCore.value ++
        Circe.value ++
        MUnit.value ++
        In(Test)(LucumaUITestkit.value) ++
        In(Test)(CrystalTestkit.value)
  )
  .dependsOn(observe_model.js)

lazy val observe_web_client = project
  .in(file("modules/web/client"))
  .settings(lucumaGlobalSettings: _*)
  .settings(esModule: _*)
  .enablePlugins(ScalaJSPlugin, LucumaCssPlugin, CluePlugin, BuildInfoPlugin)
  .settings(
    Test / test      := {},
    coverageEnabled  := false,
    libraryDependencies ++=
      Kittens.value ++
        Clue.value ++
        ClueScalaJs.value ++
        Fs2.value ++
        Http4sClient.value ++
        Http4sDom.value ++
        Crystal.value ++
        LucumaUI.value ++
        LucumaSchemas.value ++
        ScalaJsReact.value ++
        Cats.value ++
        CatsEffect.value ++
        LucumaReact.value ++
        Monocle.value ++
        LucumaCore.value ++
        Log4CatsLogLevel.value,
    scalacOptions ~= (_.filterNot(Set("-Vtype-diffs"))),
    buildInfoKeys    := Seq[BuildInfoKey](
      scalaVersion,
      sbtVersion,
      git.gitHeadCommit,
      "buildDateTime" -> System.currentTimeMillis()
    ),
    buildInfoPackage := "observe.ui",
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
  .settings(
    buildJsModule / fileInputs += (Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value.toGlob,
    buildJsModule := {
      if ((Process("npx" :: "vite" :: "build" :: Nil, baseDirectory.value) !) != 0)
        throw new Exception("Error building web client")
      else
        baseDirectory.value / "deploy" // Must match directory declared in vite.config.js
    },
    buildJsModule := buildJsModule.dependsOn(Compile / fullLinkJS).value
  )
  .dependsOn(observe_model.js, observe_ui_model)

// List all the modules and their inter dependencies
lazy val observe_server = project
  .in(file("modules/server_new"))
  .enablePlugins(GitBranchPrompt, BuildInfoPlugin, CluePlugin)
  .settings(observeCommonSettings: _*)
  .settings(
    libraryDependencies ++=
      Http4sCirce.value ++
        Http4sXml.value ++
        Log4Cats.value ++
        PPrint.value ++
        Clue.value ++
        ClueHttp4s.value ++
        ClueNatchez.value ++
        CatsParse.value ++
        Acm.value ++
        GiapiScala.value ++
        Coulomb.value ++
        LucumaSchemas.value ++
        MUnit.value ++
        Http4sServer.value ++
        Http4sJdkClient.value ++
        PureConfig.value ++
        Monocle.value ++
        Circe.value ++
        Natchez.value ++
        CatsEffect.value ++
        In(Test)(Log4CatsNoop.value),
    headerSources / excludeFilter := HiddenFileFilter || (file(
      "modules/server_new"
    ) / "src/main/scala/pureconfig/module/http4s/package.scala").getName
  )
  .settings(
    buildInfoUsePackageAsPath := true,
    buildInfoKeys ++= Seq[BuildInfoKey](name, version),
    buildInfoObject           := "OcsBuildInfo",
    buildInfoPackage          := "observe.server"
  )
  .dependsOn(observe_model.jvm % "compile->compile;test->test")
  .settings(
    unmanagedSources / excludeFilter := (unmanagedSources / excludeFilter).value
      || (Compile / sourceDirectory).value + "/scala/observe/server/flamingos2/*"
      || (Compile / sourceDirectory).value + "/scala/observe/server/ghost/*"
      || (Compile / sourceDirectory).value + "/scala/observe/server/gnirs/*"
      || (Compile / sourceDirectory).value + "/scala/observe/server/gpi/*"
      || (Compile / sourceDirectory).value + "/scala/observe/server/gsaoi/*"
      || (Compile / sourceDirectory).value + "/scala/observe/server/nifs/*"
      || (Compile / sourceDirectory).value + "/scala/observe/server/niri/*"
  )

// Unfortunately crossProject doesn't seem to work properly at the module/build.sbt level
// We have to define the project properties at this level
lazy val observe_model = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/model"))
  .enablePlugins(GitBranchPrompt)
  .settings(
    libraryDependencies ++=
      Mouse.value ++
        CatsTime.value ++
        Http4sCore.value ++
        Http4sCirce.value ++
        Http4sLaws.value ++
        LucumaOdbSchema.value ++
        Coulomb.value ++
        MUnit.value ++
        Monocle.value ++
        LucumaCore.value ++
        Circe.value ++
        In(Test)(CoulombTestkit.value) ++
        In(Test)(Discipline.value) ++
        In(Test)(CatsEffectLaws.value) ++
        In(Test)(CatsEffectTestkit.value)
  )
  .jvmSettings(observeCommonSettings)
  .jsSettings(
    // And add a custom one
    libraryDependencies ++=
      JavaTimeJs.value ++
        In(Test)(LucumaUITestkit.value),
    coverageEnabled := false
  )

/**
 * Mappings common to applications, including configurations and web application.
 */
lazy val deployedAppMappings = Seq(
  Universal / mappings ++= {
    val clientDir: File                         = (observe_web_client / buildJsModule).value
    val clientMappings: Seq[(File, String)]     =
      directory(clientDir).flatMap(path =>
        // Don't include environment confs, if present.
        if (path._2.endsWith(".conf.json")) None
        else Some(path._1 -> ("app/" + path._1.relativeTo(clientDir).get.getPath))
      )
    val siteConfigDir: File                     = (ThisProject / baseDirectory).value / "conf"
    val siteConfigMappings: Seq[(File, String)] = directory(siteConfigDir).map(path =>
      path._1 -> ("conf/" + path._1.relativeTo(siteConfigDir).get.getPath)
    )
    clientMappings ++ siteConfigMappings
  }
)

/**
 * Settings for Observe in Linux
 */
lazy val observeLinux = Seq(
  // User/Group for execution
  Linux / daemonUser     := "software",
  Linux / daemonGroup    := "software",
  Universal / maintainer := "Software Group <software@gemini.edu>",
  // This lets us build RPMs from snapshot versions
  Linux / name           := "Observe Server",
  Linux / version        := {
    (ThisBuild / version).value.replace("-SNAPSHOT", "").replace("-", "_").replace(" ", "")
  }
)

/**
 * Project for the observe server app for development
 */
lazy val deploy = project
  .in(file("deploy"))
  .enablePlugins(NoPublishPlugin)
  .enablePlugins(LucumaDockerPlugin)
  .enablePlugins(JavaServerAppPackaging)
  .enablePlugins(GitBranchPrompt)
  .dependsOn(observe_web_server)
  .settings(deployedAppMappings: _*)
  .settings(observeCommonSettings: _*)
  .settings(
    description          := "Observe Server",
    Docker / packageName := "gpp-obs",
    // Main class for launching
    Compile / mainClass  := Some("observe.web.server.http4s.WebServerLauncher"),
    dockerExposedPorts ++= Seq(9090, 9091), // Must match deployed app.conf web-server.port
    // Name of the launch script
    executableScriptName     := "observe-server",
    // Specify a different name for the config file
    bashScriptConfigLocation := Some("${app_home}/../conf/launcher.args"),
    bashScriptExtraDefines += """addJava "-Dlogback.configurationFile=${app_home}/../conf/$SITE/logback.xml""""
  )
