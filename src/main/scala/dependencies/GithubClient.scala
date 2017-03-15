package dependencies

import cats.free.Free._
import cats.implicits._
import github4s.Github
import github4s.GithubResponses.GHResult
import github4s.free.domain._

class GithubClient(owner: String, repo: String, accessToken: String) {

  val issueTitle = "Update dependency"
  val issueLabel = "update-dependencies"

  private[this] val gh = Github(Some(accessToken))

  def createIssues(list: List[DependencyUpdate]): GithubOpsLog[List[Issue]] = {

    def createIssueForDep(dep: DependencyUpdate, issues: Map[String, Issue]): GithubOpsLog[Issue] = {
      for {
        _ <- logW(s"Preparing issue for module `${dep.moduleName}`\n")
        maybeIssue = issues.get(dep.moduleName)
        issue <- maybeIssue.fold({
          logW("Existing issue not found, creating a new one\n") *> createIssue(dep)
        })({ (issue: Issue) =>
          logW(s"Found existing open issue (#${issue.number}), updating it\n") *> updateIssue(
            issue,
            dep)
        })
      } yield issue
    }

    for {
      issues        <- findIssuesByModuleName()
      createdIssues <- list.traverse(createIssueForDep(_, issues))
    } yield createdIssues
  }

  def findIssuesByModuleName(): GithubOpsLog[Map[String, Issue]] = {

    def readIssues(issues: List[Issue]): Map[String, Issue] =
      (for {
        issue <- issues
        if issue.title.startsWith(issueTitle) && issue.title.length > issueTitle.length + 1
        moduleName = issue.title.substring(issueTitle.length + 1)
      } yield moduleName -> issue).toMap

    val searchParams = List(OwnerParamInRepository(s"$owner/$repo"),
                            IssueTypeIssue,
                            IssueStateOpen,
                            SearchIn(Set(SearchInTitle)),
                            LabelParam(issueLabel))

    liftLog(liftResponse(gh.issues.searchIssues(issueTitle, searchParams)).map { response =>
      val map = readIssues(response.result.items)
      GHResult(map, response.statusCode, response.headers)
    })
  }

  def createIssue(dependencyUpdate: DependencyUpdate): GithubOpsLog[Issue] =
    liftLog(
      liftResponse(
        gh.issues.createIssue(owner = owner,
                              repo = repo,
                              title = title(dependencyUpdate),
                              body = body(dependencyUpdate),
                              labels = List(issueLabel),
                              assignees = List.empty)))

  def updateIssue(issue: Issue, dependencyUpdate: DependencyUpdate): GithubOpsLog[Issue] =
    liftLog(
      liftResponse(
        gh.issues.editIssue(
          owner = owner,
          repo = repo,
          issue = issue.number,
          state = "open",
          title = title(dependencyUpdate),
          body = body(dependencyUpdate),
          milestone = None,
          labels = List(issueLabel),
          assignees = issue.assignee.toList.map(_.login)
        )))

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
