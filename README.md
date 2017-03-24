[![Build Status](https://travis-ci.org/47deg/sbt-dependencies.svg?branch=master)](https://travis-ci.org/47deg/sbt-dependencies)

# sbt-dependencies

**sbt-dependencies** is an SBT plugin that allows to keep your project dependencies up-to-date.

This plugin depends on the [`sbt-updates`](https://github.com/rtimush/sbt-updates) plugin

## Installation

Add the following line to `project/plugins.sbt`:

```scala
addSbtPlugin("com.47deg" % "sbt-dependencies" % "0.0.4")
```

## Configuration

**SBT Configuration**

* `githubOwner`: The GitHub owner (user or organization) of the current project
* `githubRepo`: The GitHub repo name
* `githubToken`: A GitHub token to search and edit issues. You can create a new token in your [GitHub settings](https://github.com/settings/tokens/new?scopes=repo&description=sbt-dependencies) and pass it to SBT with a system property.

For example (SBT configuration):

```
githubOwner := "47deg"
githubRepo  := "sbt-dependencies"
githubToken := sys.props.get("githubToken").getOrElse("")
```

And launch SBT passing the property:

```bash
$ sbt -DgithubToken=XXXXXX
```

## SBT Tasks

* `showDependencyUpdates`: Shows the dependency updates
* `updateDependencyIssues`: Iterates over each dependency update and creates a new GitHub Issue for that dependency. If there is already an open issue for that dependency just updates it. The body of the issue contains the `patch`, `minor` and `major` versions available for that library.

## Travis configuration

You can configure this plugin for automatically update your Issues and keep this information available in your GitHub project site. To do so:

* Create a new [GitHub token]((https://github.com/settings/tokens/new?scopes=repo&description=sbt-dependencies)) with `repo` scope
* Create a new Environment Variable in your Travis project settings with that token (call it for example `GITHUB_TOKEN`)
* Put this in your SBT configuration:
```
githubToken := Option(System.getenv().get("GITHUB_TOKEN")).getOrElse("")
```
* Add the follow snippet in your `.travis.yml`:

```yaml
after_success:
  - test $TRAVIS_PULL_REQUEST == "false" && test $TRAVIS_BRANCH == "master" && sbt updateDependencyIssues
```

This will update all your dependency update issues after each merge into master.
