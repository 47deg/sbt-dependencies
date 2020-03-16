import com.typesafe.sbt.site.SitePlugin.autoImport._
import microsites.MicrositesPlugin.autoImport._
import sbt.Keys._
import sbt._
import sbt.ScriptedPlugin.autoImport._
import sbtorgpolicies.OrgPoliciesPlugin
import sbtorgpolicies.OrgPoliciesPlugin.autoImport._
import sbtorgpolicies.model._
import sbtorgpolicies.templates.badges._

object ProjectPlugin extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements

  override def requires: Plugins = OrgPoliciesPlugin

  object autoImport {

    lazy val pluginSettings = Seq(
      sbtPlugin := true,
      scalaVersion := scalac.`2.12`,
      crossScalaVersions := Seq(scalac.`2.12`),
      libraryDependencies ++= Seq(%%("github4s", "0.19.0"), %%("org-policies-core", "0.9.4")),
      addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.5.0"),
      scriptedLaunchOpts := {
        scriptedLaunchOpts.value ++
          Seq(
            "-Xmx2048M",
            "-XX:ReservedCodeCacheSize=256m",
            "-XX:+UseConcMarkSweepGC",
            "-Dplugin.version=" + version.value,
            "-Dscala.version=" + scalaVersion.value
          )
      }
    )

    lazy val micrositeSettings = Seq(
      micrositeName := "sbt-dependencies",
      micrositeDescription := "An SBT plugin that allows to you to keep your project dependencies up-to-date",
      micrositeBaseUrl := "sbt-dependencies",
      micrositeCompilingDocsTool := WithTut,
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

  }

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      name := "sbt-dependencies",
      orgGithubSetting := GitHubSettings(
        organization = "47degrees",
        project = (name in LocalRootProject).value,
        organizationName = "47 Degrees",
        groupId = "com.47deg",
        organizationHomePage = url("http://47deg.com"),
        organizationEmail = "hello@47deg.com"
      ),
      scalaVersion := "2.12.10",
      startYear := Option(2017),
      orgMaintainersSetting := List(Dev("fedefernandez", Some("Fede Fernández"))),
      orgBadgeListSetting := List(
        TravisBadge.apply(_),
        MavenCentralBadge.apply(_),
        LicenseBadge.apply(_),
        GitHubIssuesBadge.apply(_)
      )
    )
}
