---
layout: docs
title: Integrates with Travis
---

# Travis configuration

You can configure this plugin for automatically update your Issues and keep this information available in your GitHub project site. To do so:

* Create a new [GitHub token]((https://github.com/settings/tokens/new?scopes=repo&description=sbt-dependencies)) with `repo` scope
* Create a new Environment Variable in your Travis project settings with that token (call it for example `GITHUB_TOKEN`)
* Put this in your SBT configuration:
```
depGithubTokenSetting := Option(System.getenv().get("GITHUB_TOKEN"))
```
* Add the follow snippet in your `.travis.yml`:

```yaml
after_success:
  - test $TRAVIS_PULL_REQUEST == "false" && test $TRAVIS_BRANCH == "master" && sbt depUpdateDependencyIssues
```

This will update all your dependency update issues after each merge into master.