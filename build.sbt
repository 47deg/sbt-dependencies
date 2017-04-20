import sbtorgpolicies.model._

pgpPassphrase := Some(getEnvVar("PGP_PASSPHRASE").getOrElse("").toCharArray)
pgpPublicRing := file(s"$gpgFolder/pubring.gpg")
pgpSecretRing := file(s"$gpgFolder/secring.gpg")

lazy val pluginSettings = Seq(
  name := "sbt-dependencies",
  sbtPlugin := true,
  scalaVersion := "2.10.6",
  crossScalaVersions := Seq("2.10.6"),
  scalaOrganization := "org.scala-lang",
  startYear := Option(2017),
  orgGithubTokenSetting := "GITHUB_TOKEN",
  orgMaintainersSetting := List(Dev("fedefernandez", Some("Fede Fernández"))),
  orgScriptTaskListSetting := List(
    orgValidateFiles.toOrgTask,
    (orgCompile in ThisBuild).toOrgTask(allModulesScope = true, crossScalaVersionsScope = false)
  ),
  libraryDependencies ++= Seq(
    %%("github4s"),
    "com.47deg" %% "org-policies-core" % "0.4.12"),
  addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.0")
)

lazy val micrositeSettings = Seq(
  micrositeName := "sbt-dependencies",
  micrositeDescription := "An SBT plugin that allows to you to keep your project dependencies up-to-date",
  micrositeBaseUrl := "sbt-dependencies",
  micrositeDocumentationUrl := "/sbt-dependencies/docs/",
  micrositeGithubOwner := "47deg",
  micrositeGithubRepo := "sbt-dependencies",
  micrositeHighlightTheme := "darcula",
  micrositePalette := Map(
    "brand-primary"   -> "#005b96",
    "brand-secondary" -> "#03396c",
    "brand-tertiary"  -> "#011f4b",
    "gray-dark"       -> "#48474C",
    "gray"            -> "#8D8C92",
    "gray-light"      -> "#E3E2E3",
    "gray-lighter"    -> "#F4F3F9",
    "white-color"     -> "#FFFFFF"
  ),
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.md"
)


lazy val `sbt-dependencies` = (project in file("."))
  .settings(pluginSettings: _*)

lazy val docs = (project in file("docs"))
  .settings(moduleName := "docs")
  .settings(scalaOrganization := "org.scala-lang")
  .settings(micrositeSettings: _*)
  .settings(noPublishSettings: _*)
  .enablePlugins(MicrositesPlugin)
