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
  orgGithubTokenSetting := getEnvVar("GITHUB_TOKEN"),
  orgMaintainersSetting := List(Dev("fedefernandez", Some("Fede Fernández"))),
  libraryDependencies ++= Seq(
    %%("github4s"),
    "com.47deg" %% "org-policies-core" % "0.3.0"),
  addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.0")
)

lazy val `sbt-dependencies` = (project in file("."))
  .settings(pluginSettings: _*)
