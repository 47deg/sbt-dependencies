import sbtorgpolicies.model.Dev

pgpPassphrase := Some(getEnvVar("PGP_PASSPHRASE").getOrElse("").toCharArray)
pgpPublicRing := file(s"$gpgFolder/pubring.gpg")
pgpSecretRing := file(s"$gpgFolder/secring.gpg")

lazy val pluginSettings = Seq(
  name := "sbt-dependencies",
  sbtPlugin := true,
  scalaVersion := "2.10.6",
  crossScalaVersions := Seq("2.10.6"),
  scalaOrganization := "org.scala-lang",
  githubToken := getEnvVar("GITHUB_TOKEN"),
  orgMaintainersSettings := List(Dev("fedefernandez", Some("Fede Fernández"))),
  libraryDependencies ++= Seq(%%("github4s")),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.0"),
  addSbtPlugin(
    "com.47deg" % "sbt-org-policies" % "0.2.2-SNAPSHOT" % "compile"
      exclude ("com.47deg", "sbt-dependencies")
      exclude ("com.47deg", "sbt-microsites"))
)

lazy val `sbt-dependencies` = (project in file("."))
  .settings(pluginSettings: _*)
