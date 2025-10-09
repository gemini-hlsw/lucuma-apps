import _root_.cats.effect.kernel.syntax.resource
import Dependencies.*
import Versions.*
import sbt.Keys.*
import NativePackagerHelper.*
import org.scalajs.linker.interface.ModuleSplitStyle
import scala.sys.process.*
import sbt.nio.file.FileTreeView

name := "lucuma-apps"

ThisBuild / tlBaseVersion       := "0.174"
ThisBuild / tlCiReleaseBranches := Seq("main")

ThisBuild / description                         := "Lucuma Apps"
Global / onChangedBuildSource                   := ReloadOnSourceChanges
ThisBuild / scalafixDependencies += "edu.gemini" % "lucuma-schemas_3" % lucumaUiSchemas
ThisBuild / turbo                               := true
ThisBuild / scalaVersion                        := "3.7.3"
ThisBuild / crossScalaVersions                  := Seq("3.7.3")
ThisBuild / scalacOptions ++= Seq("-language:implicitConversions", "-explain-cyclic")
ThisBuild / scalacOptions ++= Seq(
  // ScalablyTyped macros introduce deprecated methods, this silences those warnings
  "-Wconf:msg=linkingInfo in package scala.scalajs.runtime is deprecated:s"
)

// TODO REMOVE ONCE THIS WORKS AGAIN
ThisBuild / tlCiScalafmtCheck := false
ThisBuild / tlCiScalafixCheck := false

ThisBuild / lucumaCssExts += "svg"

ThisBuild / resolvers := List(Resolver.mavenLocal)

// Gemini repository
ThisBuild / resolvers += "Gemini Repository".at(
  "https://github.com/gemini-hlsw/maven-repo/raw/master/releases"
)

ThisBuild / evictionErrorLevel := Level.Info

// Uncomment for local gmp testing
// ThisBuild / resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

enablePlugins(GitBranchPrompt, NoPublishPlugin)

// Build JS module for deployment, only used for observe web client
val buildJsModule = taskKey[File]("Build JS module for deployment")

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

// BEGIN SCHEMAS

// For publishing packages to NPM
lazy val createNpmProject = taskKey[Unit]("Create NPM project, package.json and files")
lazy val npmPublish       = taskKey[Unit]("Run npm publish")

lazy val schemas_model =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("schemas/model"))
    .settings(
      name := "lucuma-schemas-model",
      libraryDependencies ++=
        Circe.value ++
          CirceRefined.value ++
          Kittens.value ++
          LucumaCore.value ++
          LucumaOdbSchema.value
    )

lazy val schemas_testkit =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("schemas/testkit"))
    .dependsOn(schemas_model)
    .settings(
      name := "lucuma-schemas-testkit",
      libraryDependencies ++= LucumaCore.value
    )

lazy val schemas_tests =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Full)
    .in(file("schemas/tests"))
    .dependsOn(schemas_testkit)
    .enablePlugins(NoPublishPlugin, LucumaAppPlugin)
    .settings(
      libraryDependencies ++=
        In(Test)(
          MUnit.value ++
            Discipline.value
        )
    )

lazy val schemas_lib =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("schemas/lib"))
    .dependsOn(schemas_model)
    .enablePlugins(CluePlugin) // , LucumaLibPlugin)
    .settings(
      name                          := "lucuma-schemas",
      libraryDependencies ++=
        In(Test)(
          Fs2Io.value ++
            MUnit.value ++
            MUnitCatsEffect.value
        ),
      Compile / clueSourceDirectory := (ThisBuild / baseDirectory).value / "schemas" / "lib" / "src" / "clue",
      // Include schema files in jar.
      Compile / unmanagedResourceDirectories += (Compile / clueSourceDirectory).value / "resources",
      createNpmProject              := {
        val npmDir = target.value / "npm"

        val schemaFile =
          (Compile / clueSourceDirectory).value / "resources" / "lucuma" / "schemas" / "ObservationDB.graphql"

        IO.write(
          npmDir / "package.json",
          s"""|{
             |  "name": "lucuma-schemas",
             |  "version": "${version.value}",
             |  "license": "${licenses.value.head._1}",
             |  "exports": {
             |    "./odb": "./${schemaFile.getName}"
             |  },
             |  "repository": {
             |    "type": "git",
             |    "url": "git+https://github.com/gemini-hlsw/lucuma-apps.git"
             |  }
             |}
             |""".stripMargin
        )

        IO.copyFile(schemaFile, npmDir / schemaFile.getName)

        streams.value.log.info(s"Created NPM project in ${npmDir}")
      },
      npmPublish                    := {
        import scala.sys.process._
        val npmDir = target.value / "npm"

        val _ = createNpmProject.value
        Process(List("npm", "publish"), npmDir).!!
      }
    )
    .jsSettings(
      Test / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
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
    LucumaCatalogTestkit.value
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

lazy val explore_model = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("explore/model"))
  .dependsOn(schemas_lib)
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonLibSettings: _*)
  .jvmSettings(exploreCommonJvmSettings)
  .jsSettings(exploreCommonJsLibSettings)

lazy val explore_modelTestkit = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("explore/model-testkit"))
  .dependsOn(explore_model, schemas_testkit)
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonLibSettings: _*)
  .settings(exploreTestkitLibSettings: _*)
  .jsSettings(exploreCommonModuleTest: _*)
  .jvmSettings(exploreCommonJvmSettings)

lazy val explore_modelTests = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("explore/model-tests"))
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
  .settings(esModule: _*)
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
  .dependsOn(explore_model.js, schemas_lib.js, explore_modelTestkit.js % Test)
  .enablePlugins(ScalaJSPlugin, BuildInfoPlugin, LucumaAppPlugin)
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonJsLibSettings: _*)
  .settings(exploreCommonModuleTest: _*)
  .settings(
    libraryDependencies ++=
      LucumaSsoFrontendClient.value ++
        LucumaCatalog.value ++
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

lazy val explore_app: Project = project
  .in(file("explore/app"))
  .dependsOn(explore_model.js, explore_common)
  .settings(exploreCommonSettings: _*)
  .settings(exploreCommonJsLibSettings: _*)
  .settings(esModule: _*)
  .enablePlugins(ScalaJSPlugin, LucumaCssPlugin, CluePlugin, LucumaAppPlugin)
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
      .value,
    buildJsModule        := {
      val jsFiles = (Compile / fullLinkJSOutput).value
      if (sys.env.getOrElse("POST_STAGE_CLEAN", "false").equals("true")) {
        println("Cleaning up...")
        // Remove coursier cache
        val coursierCacheDir = csrCacheDirectory.value
        sbt.IO.delete(coursierCacheDir)
      }
      jsFiles
    }
  )

// START OBSERVE

lazy val observeCommonSettings = Seq(
  Compile / packageDoc / mappings := Seq(),
  Compile / doc / sources         := Seq.empty,
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val observe_web_server = project
  .in(file("modules/web/server"))
  .enablePlugins(BuildInfoPlugin, LucumaAppPlugin)
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
  .dependsOn(schemas_lib.js)
  .enablePlugins(ScalaJSPlugin)
  .settings(lucumaGlobalSettings: _*)
  .settings(
    coverageEnabled := false,
    libraryDependencies ++=
      Crystal.value ++
        LucumaUI.value ++
        LucumaCore.value ++
        Circe.value ++
        MUnit.value ++
        In(Test)(
          LucumaUITestkit.value ++
            CrystalTestkit.value
        )
  )
  .dependsOn(observe_model.js)

lazy val observe_web_client = project
  .in(file("modules/web/client"))
  .dependsOn(schemas_lib.js)
  .enablePlugins(ScalaJSPlugin, LucumaCssPlugin, CluePlugin, BuildInfoPlugin, LucumaAppPlugin)
  .settings(lucumaGlobalSettings: _*)
  .settings(esModule: _*)
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
  .dependsOn(schemas_lib.jvm)
  .enablePlugins(BuildInfoPlugin, CluePlugin, LucumaAppPlugin)
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
        In(Test)(
          CoulombTestkit.value ++
            Discipline.value ++
            CatsEffectLaws.value ++
            CatsEffectTestkit.value
        )
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
lazy val observe_deploy = project
  .in(file("modules/deploy"))
  .enablePlugins(LucumaDockerPlugin, JavaServerAppPackaging)
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

// BEGIN ALIASES

val lintCSS = TaskKey[Unit]("lintCSS", "Lint CSS files")
lintCSS := {
  if (("npm run lint-dark" #&& "npm run lint-light" !) != 0)
    throw new Exception("Error in CSS format")
}

val fixCSS = TaskKey[Unit]("fixCSS", "Fix CSS files")
fixCSS := {
  if (("npm run fix-dark" #&& "npm run fix-light" !) != 0)
    throw new Exception("Error in CSS fix")
}

addCommandAlias(
  "quickTest",
  "explore_modelTestsJVM/test"
)

addCommandAlias(
  "fixImports",
  "; scalafix OrganizeImports; Test/scalafix OrganizeImports"
)

addCommandAlias(
  "fix",
  "; prePR; fixCSS"
)

// BEGIN GITHUB ACTIONS

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

ThisBuild / githubWorkflowGeneratedUploadSteps := Seq.empty
ThisBuild / githubWorkflowSbtCommand           := "sbt -v -J-Xmx6g"
ThisBuild / githubWorkflowEnv += faNpmAuthToken
ThisBuild / githubWorkflowEnv += herokuToken

// https://github.com/actions/setup-node/issues/835#issuecomment-1753052021
lazy val exploreSetupNodeNpmInstall =
  List(
    WorkflowStep.Use(
      UseRef.Public("actions", "setup-node", "v4"),
      name = Some("Explore Setup Node.js"),
      params = Map(
        "node-version"          -> "20",
        "cache"                 -> "npm",
        "cache-dependency-path" -> "explore/package-lock.json"
      )
    ),
    // Explore NPM cache
    WorkflowStep.Use(
      UseRef.Public("actions", "cache", "v4"),
      name = Some("Cache Explore node_modules"),
      id = Some("explore-cache-node_modules"),
      params = {
        val prefix = "node_modules"
        val key    = s"$prefix-$${{ hashFiles('explore/package-lock.json') }}"
        Map("path" -> "node_modules", "key" -> key, "restore-keys" -> prefix)
      }
    ),
    WorkflowStep.Run(
      List("cd explore", "npm clean-install --verbose"),
      name = Some("npm clean-install"),
      cond = Some("steps.explore-cache-node_modules.outputs.cache-hit != 'true'")
    )
  )

  lazy val observeSetupNodeNpmInstall =
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
      // Observe NPM cache
      WorkflowStep.Use(
        UseRef.Public("actions", "cache", "v4"),
        name = Some("Cache Observe node_modules"),
        id = Some("observe-cache-node_modules"),
        params = {
          val prefix = "node_modules"
          val key    = s"$prefix-$${{ hashFiles('modules/web/client/package-lock.json') }}"
          Map("path" -> "node_modules", "key" -> key, "restore-keys" -> prefix)
        }
      ),
      WorkflowStep.Run(
        List("cd modules/web/client", "npm clean-install --verbose"),
        name = Some("npm clean-install"),
        cond = Some("steps.observe-cache-node_modules.outputs.cache-hit != 'true'")
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
    List("clean", "observe_deploy/docker:publish"),
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

lazy val exploreSbtLink =
  WorkflowStep.Sbt(List("explore_app/buildJsModule"), name = Some("Link Explore"))

lazy val exploreNpmBuild = WorkflowStep.Run(
  List("cd explore", "npm run build"),
  name = Some("Build Explore"),
  env = Map("NODE_OPTIONS" -> "--max-old-space-size=8192")
)

// https://frontside.com/blog/2020-05-26-github-actions-pull_request/#how-does-pull_request-affect-actionscheckout
lazy val overrideCiCommit = WorkflowStep.Run(
  List("""echo "CI_COMMIT_SHA=${{ github.event.pull_request.head.sha}}" >> $GITHUB_ENV"""),
  name = Some("override CI_COMMIT_SHA"),
  cond = Some(prCond)
)

// lazy val bundlemon = WorkflowStep.Use(
//   UseRef.Public("lironer", "bundlemon-action", "v1"),
//   name = Some("Run BundleMon")
// )

def firebaseDeploy(name: String, cond: String, live: Boolean) = WorkflowStep.Use(
  UseRef.Public("FirebaseExtended", "action-hosting-deploy", "v0"),
  name = Some(name),
  cond = Some(cond),
  params = Map(
    "repoToken"              -> "${{ secrets.GITHUB_TOKEN }}",
    "firebaseServiceAccount" -> "${{ secrets.FIREBASE_SERVICE_ACCOUNT_EXPLORE_GEMINI }}",
    "projectId"              -> "explore-gemini",
    "target"                 -> "dev",
    "entryPoint"             -> "./explore"
  ) ++ (if (live) Map("channelId" -> "live") else Map.empty)
)

// lazy val firebaseDeployReview = firebaseDeploy(
//   "Deploy review app to Firebase",
//   allConds(
//     prCond,
//     notDependabotCond,
//     "github.event.pull_request.head.repo.full_name == github.repository"
//   ),
//   live = false
// )

lazy val firebaseDeployDev = firebaseDeploy(
  "Deploy staging app to Firebase",
  mainCond,
  live = true
)

lazy val recordDeploymentMetadata = WorkflowStep.Run(
  List(
    "# Create a deployment record with commit SHA for tracking",
    """echo "Recording deployment: ${{ github.sha }} to explore-gemini-dev"""",
    """curl -X POST https://api.github.com/repos/${{ github.repository }}/deployments -H "Authorization: Bearer ${{ secrets.GITHUB_TOKEN }}" -H "Accept: application/vnd.github+json" -d '{ "ref": "${{ github.sha }}", "environment": "development", "description": "Explore deployment to dev", "auto_merge": false, "required_contexts": [], "task": "deploy:Explore" }' """
  ),
  name = Some("Record deployment SHA"),
  cond = Some(mainCond)
)

ThisBuild / githubWorkflowBuildPreamble ++= exploreSetupNodeNpmInstall

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    "observe-deploy",
    "Build and publish Observe Docker image / Deploy to Heroku",
    githubWorkflowJobSetup.value.toList :::
      observeSetupNodeNpmInstall :::
      dockerHubLogin ::
      sbtDockerPublish ::
      herokuRelease ::
      Nil,
    scalas = List(scalaVersion.value),
    javas = githubWorkflowJavaVersions.value.toList.take(1),
    cond = Some(allConds(mainCond, geminiRepoCond))
  )

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    "explore-deploy",
    "Build and deploy Explore",
    githubWorkflowJobSetup.value.toList :::
      exploreSetupNodeNpmInstall :::
      exploreSbtLink ::
      exploreNpmBuild ::
      overrideCiCommit ::
      // bundlemon ::
      // firebaseDeployReview ::
      firebaseDeployDev ::
      recordDeploymentMetadata ::
      Nil,
    // Only 1 scalaVersion, so no need for matrix
    sbtStepPreamble = Nil,
    scalas = Nil,
    javas = githubWorkflowJavaVersions.value.toList.take(1),
    cond = Some(allConds(anyConds(mainCond, prCond), geminiRepoCond))
  )

ThisBuild / githubWorkflowPublishPreamble +=
  WorkflowStep.Use(
    UseRef.Public("actions", "setup-node", "v4"),
    Map(
      "node-version" -> "24",
      "registry-url" -> "https://registry.npmjs.org",
      "cache"        -> "npm"
    )
  )

ThisBuild / githubWorkflowPublish ++= Seq(
  WorkflowStep.Sbt(
    // List("css/npmPublish", "schemas_libJVM/npmPublish"),
    List("schemas_libJVM/npmPublish"),
    name = Some("NPM Publish"),
    env = Map("NODE_AUTH_TOKEN" -> s"$${{ secrets.NPM_REPO_TOKEN }}"),
    cond = Some("startsWith(github.ref, 'refs/tags/v')")
  )
)
