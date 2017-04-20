/*
 * Copyright 2017 47 Degrees, LLC. <http://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package dependencies

import com.timushev.sbt.updates.UpdatesPlugin.autoImport._
import github4s.Github._
import github4s.GithubResponses.GHResult
import github4s.jvm.Implicits._
import sbt.Keys._
import sbt._
import sbtorgpolicies.github.instances._

import scala.util.{Failure, Success, Try}
import scalaj.http.HttpResponse

object DependenciesPlugin extends AutoPlugin {

  object autoImport extends DependenciesKeys

  import DependenciesPlugin.autoImport._

  lazy val defaultSettings = Seq(
    depShowDependencyUpdates := {
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
    depUpdateDependencyIssues := {
      val list = Updates.readUpdates(dependencyUpdatesData.value)
      streams.value.log.info("Reading GitHub issues\n")
      if (depGithubTokenSetting.value.isDefined) {
        val client =
          GithubClient(
            depGithubOwnerSetting.value,
            depGithubRepoSetting.value,
            depGithubTokenSetting.value)
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
            |  depGithubTokenSetting := Option(System.getenv().get("GITHUB_TOKEN"))
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
    dependencyUpdatesExclusions :=
      moduleFilter("org.scala-sbt", "sbt-launch") |
        moduleFilter("org.scala-lang"),
    depGithubTokenSetting := Option(System.getenv().get("GITHUB_TOKEN"))
  )

  override val projectSettings: Seq[Setting[_]] = defaultSettings

}
