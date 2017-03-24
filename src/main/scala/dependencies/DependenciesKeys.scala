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
