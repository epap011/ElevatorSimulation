scalaVersion := "2.13.10"
val AkkaVersion = "2.8.2"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test
)