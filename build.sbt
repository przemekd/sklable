val scalaV            = "2.13.5"
val akkaV             = "2.6.12"
val akkaHttpV         = "10.2.3"
val upickleVersion    = "1.2.3"
val scalaJsDomVersion = "1.1.0"
val specs2Version     = "4.8.0"
val airstreamV        = "0.12.0"
val laminarV          = "0.12.1"
val waypointV         = "0.3.0"
val airframeLogV      = "20.10.0"
val sttpClientV       = "2.2.9"
val scalinguaV        = "1.0"
val scalaTestV        = "3.2.2"

ThisBuild / scapegoatVersion := "1.4.8"

lazy val root = project
  .in(file("."))
  .settings(name := "sklable", version := "0.1")
  .aggregate(frontend, backend)

// Scala-Js frontend
lazy val frontend =
  project
    .in(file("frontend"))
    .enablePlugins(ScalaJSPlugin, Scalingua)
    .settings(commonSettings: _*)
    .settings(
      scalaJSUseMainModuleInitializer := true,
      Compile / compileLocalesStrategy := "InlineBase64",
      Test / compileLocalesStrategy := "InlineBase64",
      libraryDependencies ++= Seq(
            "com.raquo"                    %%% "airstream" % airstreamV,
            "com.raquo"                    %%% "laminar"   % laminarV,
            "com.raquo"                    %%% "waypoint"  % waypointV,
            "com.softwaremill.sttp.client" %%% "core"      % sttpClientV,
            "ru.makkarpov"                 %%% "scalingua" % scalinguaV,
            "org.scalatest"                %%% "scalatest" % scalaTestV % Test
          )
    )
    .dependsOn(sharedJs)

// Akka Http based backend
lazy val backend =
  project
    .in(file("backend"))
    .settings(commonSettings: _*)
    .settings(backendDependencies: _*)
    .settings(
      Compile / resourceGenerators +=
          (Def.taskDyn {
            val env = buildEnv.value
            env match {
              case BuildEnv.Development =>
                Def.task {
                  val f1            = (frontend / Compile / fastOptJS).value.data
                  val f1SourceMap   = f1.getParentFile / (f1.getName + ".map")
                  val indexTemplate = IO.read((Compile / sourceDirectory).value / "templates" / "index.html")
                  val indexOutput   = (Compile / resourceManaged).value / "web" / "index.html"
                  IO.write(indexOutput, indexTemplate.replace("<script-name>", "frontend-fastopt.js"))
                  Seq(f1, f1SourceMap, indexOutput)
                }
              case BuildEnv.Production =>
                Def.task {
                  val f1            = (frontend / Compile / fullOptJS).value.data
                  val indexTemplate = IO.read((Compile / sourceDirectory).value / "templates" / "index.html")
                  val indexOutput   = (Compile / resourceManaged).value / "web" / "index.html"
                  IO.write(indexOutput, indexTemplate.replace("<script-name>", "frontend-opt.js"))
                  Seq(f1, indexOutput)
                }
            }
          }).taskValue
      // watchSources += WatchSource(frontend.base)
    )
    .settings(
      Universal / mappings += {
        val confFile = buildEnv.value match {
          case BuildEnv.Development => "conf/development.conf"
          case BuildEnv.Production  => "conf/production.conf"
        }
        ((Compile / sourceDirectory).value / confFile) -> "application.conf",
      },
    )
    .enablePlugins(JavaServerAppPackaging)
    .dependsOn(sharedJvm)

// Shared code
lazy val shared =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("shared"))
    .settings(commonSettings: _*)
    .settings(
      libraryDependencies += "com.lihaoyi"        %%% "upickle"      % upickleVersion,
      libraryDependencies += "org.wvlet.airframe" %%% "airframe-log" % airframeLogV,
      libraryDependencies += "org.scalatest"       %% "scalatest"    % scalaTestV % "test"
    )

lazy val sharedJvm = shared.jvm
lazy val sharedJs  = shared.js

def backendDependencies =
  Seq(
    libraryDependencies ++= Seq(
          "com.typesafe.akka"  %% "akka-stream"          % akkaV,
          "com.typesafe.akka"  %% "akka-actor"           % akkaV,
          "com.typesafe.akka"  %% "akka-stream-typed"    % akkaV,
          "org.wvlet.airframe" %% "airframe-log"         % airframeLogV,
          "org.slf4j"           % "slf4j-jdk14"          % "1.7.30",
          "com.typesafe.akka"  %% "akka-http"            % akkaHttpV,
          "com.typesafe.akka"  %% "akka-http-spray-json" % akkaHttpV,
          "com.lihaoyi"        %% "upickle"              % upickleVersion,
          "org.specs2"         %% "specs2-core"          % specs2Version % "test",
          "org.scalatest"      %% "scalatest"            % scalaTestV    % "test"
        )
  )

def commonSettings =
  Seq(
    scalaVersion := scalaV,
    scalacOptions ++= Seq("-deprecation", "-feature", "-encoding", "utf8", "-unchecked", "-Xlint")
  )

backend / Compile / mainClass := Some("server.SklableServer")
