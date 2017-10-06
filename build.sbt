import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "state monad example",
    libraryDependencies += scalaTest % Test,
    scalacOptions += "-Ypartial-unification",
    libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-MF",
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.3",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.6"
  )
