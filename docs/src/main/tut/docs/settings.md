---
layout: docs
title: Settings
---

# Configuration

* `depGithubOwnerSetting`: The GitHub owner (user or organization) of the current project
* `depGithubRepoSetting`: The GitHub repo name
* `depGithubTokenSetting`: A GitHub token to search and edit issues. You can create a new token in your [GitHub settings](https://github.com/settings/tokens/new?scopes=repo&description=sbt-dependencies) and pass it to SBT with a system property.

For example (SBT configuration):

```
depGithubOwnerSetting := "47deg"
depGithubRepoSetting  := "sbt-dependencies"
depGithubTokenSetting := sys.props.get("githubToken")
```

And launch SBT passing the property:

```bash
$ sbt -DgithubToken=XXXXXX
```

# SBT Tasks

## `depShowDependencyUpdates`

Shows the dependency updates in the console.

## `depUpdateDependencyIssues`

Iterates over each dependency update and creates a new GitHub Issue for that dependency. 
If there is already an open issue for that dependency just updates it.
It also closes the tickets that are no longer valid.
The body of the issue contains the `patch`, `minor` and `major` versions available for that library.
