import de.heikoseeberger.sbtheader.license.Apache2_0
import sbtorgpolicies._
import sbtorgpolicies.OrgPoliciesPlugin.autoImport._

val dev = Seq(Dev("47 Degrees (twitter: @47deg)", "47 Degrees"))
val gh  = GitHubSettings("com.47deg", "sbt-dependencies", "47 Degrees", apache)

lazy val artifactSettings = Seq(
  name := gh.proj,
  organization := gh.org,
  organizationName := gh.publishOrg,
  homepage := Option(url("http://www.47deg.com")),
  organizationHomepage := Some(new URL("http://47deg.com")),
  headers := Map(
    "scala" -> Apache2_0("2017", "47 Degrees, LLC. <http://www.47deg.com>")
  )
)

pgpPassphrase := Some(sys.env.getOrElse("PGP_PASSPHRASE", "").toCharArray)
pgpPublicRing := file(s"${sys.env.getOrElse("PGP_FOLDER", ".")}/pubring.gpg")
pgpSecretRing := file(s"${sys.env.getOrElse("PGP_FOLDER", ".")}/secring.gpg")

lazy val pluginSettings = Seq(
    sbtPlugin := true,
    scalaVersion in ThisBuild := "2.10.6",
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots"),
      "jgit-repo" at "http://download.eclipse.org/jgit/maven"
    ),
    libraryDependencies ++= Seq(
      "com.47deg"       %% "github4s" % "0.11.1-SNAPSHOT",
      dep("scalatest")  % "test",
      dep("scalacheck") % "test"
    ),
    scalafmtConfig in ThisBuild := Some(file(".scalafmt"))
  ) ++ reformatOnCompileSettings

lazy val commonSettings = artifactSettings ++ miscSettings

lazy val allSettings = pluginSettings ++
    commonSettings ++
    sharedPublishSettings(gh, dev)

lazy val `sbt-dependencies` = (project in file("."))
  .settings(moduleName := "sbt-dependencies")
  .settings(allSettings: _*)
  .settings(addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.0"))
