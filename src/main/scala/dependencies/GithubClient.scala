package dependencies

import cats.free._
import cats.implicits._
import github4s.Github
import github4s.GithubResponses.{GHException, GHIO, GHResponse, GHResult}
import github4s.app.GitHub4s
import github4s.free.domain._
import sbt.Logger

class GithubClient(owner: String, repo: String, accessToken: String) {

  val issueTitle = "Update dependency"
  val issueLabel = "update-dependencies"

  def createIssues(list: List[DependencyUpdate], log: Logger): GHIO[GHResponse[List[Issue]]] = {

    def createIssueForDep(dep: DependencyUpdate,
                          issues: Map[String, Issue],
                          log: Logger): GHIO[GHResponse[Issue]] = {
      log.info(s"Preparing issue for module `${dep.moduleName}`\n")
      issues.get(dep.moduleName) match {
        case Some(issue) =>
          log.info(s"Found existing open issue (#${issue.number}), updating it\n")
          updateIssue(issue, dep)
        case None =>
          log.info("Existing issue not found, creating a new one\n")
          createIssue(dep)
      }
    }

    for {
      issues <- findIssuesByModuleName()
      createdIssues <- {
        issues match {
          case Right(response) =>
            list.traverse(createIssueForDep(_, response.result, log)).map { list =>
              val errors = list.collect {
                case Left(e) => e
              }

              val createdIssues = list.collect {
                case Right(issueResult) => issueResult.result
              }

              errors.headOption match {
                case Some(e) => Left(e)
                case None    => Right(GHResult(createdIssues, 200, Map.empty))
              }

            }
          case Left(e) =>
            Free.pure[GitHub4s, Either[GHException, GHResult[List[Issue]]]](Left(e))
        }
      }
    } yield createdIssues
  }

  def findIssuesByModuleName(): GHIO[GHResponse[Map[String, Issue]]] = {

    def readIssues(issues: List[Issue]): Map[String, Issue] =
      issues.flatMap { issue =>
        if (issue.title.startsWith(issueTitle) && issue.title.length > issueTitle.length + 1) {
          val moduleName = issue.title.substring(issueTitle.length + 1)
          Option(moduleName -> issue)
        } else {
          None
        }
      }.toMap

    val searchParams = List(OwnerParamInRepository(s"$owner/$repo"),
                            IssueTypeIssue,
                            IssueStateOpen,
                            SearchIn(Set(SearchInTitle)),
                            LabelParam(issueLabel))

    Github(Some(accessToken)).issues.searchIssues(issueTitle, searchParams).map {
      case Right(response) =>
        val map = readIssues(response.result.items)
        Right(GHResult(map, response.statusCode, response.headers))
      case Left(e) => Left(e)
    }
  }

  def createIssue(dependencyUpdate: DependencyUpdate): GHIO[GHResponse[Issue]] =
    Github(Some(accessToken)).issues.createIssue(owner = owner,
                                                 repo = repo,
                                                 title = title(dependencyUpdate),
                                                 body = body(dependencyUpdate),
                                                 labels = List(issueLabel),
                                                 assignees = List.empty)

  def updateIssue(issue: Issue, dependencyUpdate: DependencyUpdate): GHIO[GHResponse[Issue]] =
    Github(Some(accessToken)).issues.editIssue(
      owner = owner,
      repo = repo,
      issue = issue.number,
      state = "open",
      title = title(dependencyUpdate),
      body = body(dependencyUpdate),
      milestone = None,
      labels = List(issueLabel),
      assignees = issue.assignee.toList.map(_.login)
    )

  def title(dependencyUpdate: DependencyUpdate): String =
    s"$issueTitle ${dependencyUpdate.moduleName}"

  def body(dependencyUpdate: DependencyUpdate): String =
    s"""
       | Actual version: ${dependencyUpdate.revision}
       | Patch: ${dependencyUpdate.patch.getOrElse("-")}
       | Minor: ${dependencyUpdate.minor.getOrElse("-")}
       | Major: ${dependencyUpdate.major.getOrElse("-")}
         """.stripMargin

}

object GithubClient {

  def apply(owner: String, repo: String, accessToken: String): GithubClient =
    new GithubClient(owner, repo, accessToken)

}
