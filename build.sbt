ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "restaurant-discovery",
    version := "1.0.0",
    libraryDependencies ++= Seq(
      "org.mongodb.scala" %% "mongo-scala-driver" % "5.1.0",
      "org.slf4j"          % "slf4j-nop"          % "2.0.13" // silences driver logging in the CLI
    ),
    // needed so the CLI can read from the keyboard when started with `sbt run`
    run / fork := true,
    run / connectInput := true,
    run / outputStrategy := Some(StdoutOutput)
  )