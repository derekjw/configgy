name := "configgy"

organization := "net.lag"

version := "2.0.5-nologgy"

resolvers += ("ccap-artifactory-scala" at "http://repoman.wicourts.gov/artifactory/scala").withAllowInsecureProtocol(true)

scalaVersion := "2.12.21"

crossScalaVersions := Seq("2.12.21")

ThisBuild / publishTo := {
  val repo =
    if (version.value.trim.endsWith("SNAPSHOT"))
      "CCAP Snapshots" at "http://repoman.wicourts.gov/artifactory/libs-snapshot-local"
    else
      "CCAP Releases" at "http://repoman.wicourts.gov/artifactory/libs-release-local"
  Some(repo.withAllowInsecureProtocol(true))
}

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

libraryDependencies += "org.specs2" %% "specs2-core" % "4.23.0" % Test

// Specs share the global Configgy.config singleton and JVM system properties.
Test / parallelExecution := false
