package dependencies

import cats.MonadError
import com.timushev.sbt.updates.UpdatesPlugin.autoImport._
import com.timushev.sbt.updates.versions.Version
import github4s.Github._
import github4s.jvm.Implicits._
import sbt.Keys._
import sbt._

import scala.collection.immutable.SortedSet
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
import scalaj.http.HttpResponse

object DependenciesPlugin extends AutoPlugin {

  object autoImport extends DependenciesKeys

  import DependenciesPlugin.autoImport._

  def readUpdates(data: Map[ModuleID, SortedSet[Version]], log: Logger)(
      f: (List[DependencyUpdate]) => Unit): Unit =
    Updates.readUpdates(data) match {
      case Nil  => log.info("\nNo dependency updates found\n")
      case list => f(list)
    }

  implicit val monadError: MonadError[Future, Throwable] =
    catsStdInstancesForFuture

  lazy val defaultSettings = Seq(
    showDependencyUpdates := {
      readUpdates(dependencyUpdatesData.value, streams.value.log) {
        list =>
          val fullTable = List("Module", "Revision", "Patch", "Minor", "Major") +:
              list.map(
                dep =>
                  List(dep.moduleName,
                       dep.revision,
                       dep.patch.getOrElse(""),
                       dep.minor.getOrElse(""),
                       dep.major.getOrElse("")))
          streams.value.log.info("\nFound some dependency updates:\n")
          streams.value.log.info(TablePrinter.format(fullTable))
          streams.value.log.info("Execute `updateDependencyIssues` to update your issues\n")
      }
    },
    updateDependencyIssues := {
      readUpdates(dependencyUpdatesData.value, streams.value.log) {
        list =>
          streams.value.log.info("Reading GitHub issues\n")
          githubToken.value match {
            case accessToken if accessToken.nonEmpty =>
              val future =
                GithubClient(githubOwner.value, githubRepo.value, accessToken)
                  .createIssues(list, streams.value.log)
                  .execFuture[HttpResponse[String]](Map("user-agent" -> "sbt-dependencies"))
              Try(Await.result(future, 1.minutes)) match {
                case Success(Right(_)) =>
                  streams.value.log.info("GitHub issues created or updated\n")
                case Success(Left(e)) =>
                  streams.value.log.error(s"Error creating issues")
                  e.printStackTrace()
                case Failure(e) =>
                  streams.value.log.error(s"Error creating issues")
                  e.printStackTrace()
              }
            case _ =>
              streams.value.log.info(
                """
                  | Can't read the access token, please set the GitHub token with the SBT setting key 'githubToken'
                  |
                  | For ex:
                  |
                  |  // build.sbt
                  |  githubToken := sys.props.get("githubToken").getOrElse("")
                  |
                  |  // Command line
                  |  `sbt -DgithubToken=XXXXXX`
                  |
                  | You need to create a token in this page with the `repo` scope:
                  |  * https://github.com/settings/tokens/new?scopes=repo&description=sbt-dependencies
                  |
                  | """.stripMargin)
          }

      }
    },
    dependencyUpdatesExclusions := moduleFilter(organization = "org.scala-lang"),
    githubToken := ""
  )

  override val projectSettings: Seq[Setting[_]] = defaultSettings

}
