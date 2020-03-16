pgpPassphrase := Some(getEnvVar("PGP_PASSPHRASE").getOrElse("").toCharArray)

lazy val `sbt-dependencies` = (project in file("."))
  .settings(moduleName := "sbt-dependencies")
  .settings(pluginSettings: _*)
  .enablePlugins(SbtPlugin)

lazy val docs = (project in file("docs"))
  .settings(moduleName := "docs")
  .settings(micrositeSettings: _*)
  .settings(noPublishSettings: _*)
  .enablePlugins(MicrositesPlugin)
