package dependencies

import sbt._

trait DependenciesKeys {

  val showDependencyUpdates: TaskKey[Unit] =
    taskKey[Unit]("Shows the dependency updates")
  val updateDependencyIssues: TaskKey[Unit] =
    taskKey[Unit]("Creates and updates issues for all dependency updates")

  val githubOwner: SettingKey[String] = settingKey[String]("GitHub owner")
  val githubRepo: SettingKey[String]  = settingKey[String]("GitHub repo")
  val githubToken: SettingKey[Option[String]] =
    settingKey[Option[String]]("GitHub token with repo scope")

}
