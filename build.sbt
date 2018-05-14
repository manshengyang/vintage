name := "vintage"
version := "0.0.1"
scalaVersion := "2.12.6"

val jvmOptions = Seq(
  "-XX:+UseG1GC",
  "-XX:MaxGCPauseMillis=200",
  "-XX:+UseCompressedOops",
  "-Xms1G",
  "-Xmx7G",
  "-Xss4m"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-Xlint",
  "-Xexperimental"
)

val log4jVersion = "2.11.0"
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.google.guava" % "guava" % "25.0-jre",
  "de.sfuhrm" % "saphir-hash-jca" % "3.0.3",

  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  "com.lmax" % "disruptor" % "3.4.2",
  "org.slf4j" % "slf4j-api" % "1.7.12",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % log4jVersion,
  "org.apache.logging.log4j" % "log4j-api" % log4jVersion,
  "org.apache.logging.log4j" % "log4j-core" % log4jVersion,
  "org.apache.logging.log4j" % "log4j-jul" % log4jVersion,
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % "2.5.0",
  "io.spray" %%  "spray-json" % "1.3.3"
)

javaOptions ++= jvmOptions ++ Seq(
  "-Djava.util.logging.manager=org.apache.logging.log4j.jul.LogManager",
  "-Dlog4j.configurationFile=conf/log4j2.yaml"
)

fork in run := true
