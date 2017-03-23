package dependencies

import com.timushev.sbt.updates.UpdatesPlugin.autoImport._
import github4s.Github._
import github4s.GithubResponses.GHResult
import github4s.jvm.Implicits._
import sbt.Keys._
import sbt._
import sbtorgpolicies.OrgPoliciesPlugin
import sbtorgpolicies.github.instances._

import scala.util.{Failure, Success, Try}
import scalaj.http.HttpResponse

object DependenciesPlugin extends AutoPlugin {

  override def requires: Plugins = OrgPoliciesPlugin

  object autoImport extends DependenciesKeys

  import DependenciesPlugin.autoImport._

  lazy val defaultSettings = Seq(
    showDependencyUpdates := {
      Updates.readUpdates(dependencyUpdatesData.value) match {
        case Nil => streams.value.log.info("\nNo dependency updates found\n")
        case list =>
          val fullTable = List("Module", "Revision", "Patch", "Minor", "Major") +:
            list.map(
            dep =>
              List(
                dep.moduleName,
                dep.revision,
                dep.patch.getOrElse(""),
                dep.minor.getOrElse(""),
                dep.major.getOrElse("")))
          streams.value.log.info("\nFound some dependency updates:\n")
          streams.value.log.info(TablePrinter.format(fullTable))
          streams.value.log
            .info("Execute `updateDependencyIssues` to update your issues\n")
      }
    },
    updateDependencyIssues := {
      val list = Updates.readUpdates(dependencyUpdatesData.value)
      streams.value.log.info("Reading GitHub issues\n")
      if (githubToken.value.isDefined) {
        val client =
          GithubClient(githubOwner.value, githubRepo.value, githubToken.value)
        val result = client
          .updateIssues(list)
          .run
          .value
          .exec[Try, HttpResponse[String]](Map("user-agent" -> "sbt-dependencies"))

        result match {
          case Success(Right(GHResult((logEntries: Log, _), _, _))) =>
            logEntries.foreach(streams.value.log.info(_))
            streams.value.log.info("GitHub issues created or updated\n")
          case Success(Left(e)) =>
            streams.value.log.error(s"Error updating issues")
            e.printStackTrace()
          case Failure(e) =>
            streams.value.log.error(s"Unexpected error updating issues")
            e.printStackTrace()
        }
      } else {
        streams.value.log.info(
          """
            | Can't read the access token, please set the GitHub token with the SBT setting key 'githubToken'
            |
            | For ex:
            |
            |  // build.sbt (default behaviour)
            |  githubToken := Option(System.getenv().get("GITHUB_TOKEN"))
            |
            |  // Command line
            |  `env GITHUB_TOKEN=XXXXX sbt`
            |
            | You would need to create a token in this page with the `repo` scope:
            |  * https://github.com/settings/tokens/new?scopes=repo&description=sbt-dependencies
            |
            | """.stripMargin)
      }

    },
    dependencyUpdatesExclusions := moduleFilter(organization = "org.scala-lang"),
    githubToken := Option(System.getenv().get("GITHUB_TOKEN"))
  )

  override val projectSettings: Seq[Setting[_]] = defaultSettings

}
