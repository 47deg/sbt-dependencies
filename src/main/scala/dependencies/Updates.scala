/*
 * Copyright 2017-2020 47 Degrees, LLC. <http://www.47deg.com>
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

import com.timushev.sbt.updates.versions.Version
import sbt.ModuleID

import scala.collection.immutable.SortedSet

object Updates {

  def formatModule(module: ModuleID): String =
    module.organization + ":" + module.name + module.configurations
      .map(":" + _)
      .getOrElse("")

  def patchUpdate(c: Version, updates: SortedSet[Version]): Option[Version] =
    updates.filter(v => v.major == c.major && v.minor == c.minor).lastOption

  def minorUpdate(c: Version, updates: SortedSet[Version]): Option[Version] =
    updates.filter(v => v.major == c.major && v.minor > c.minor).lastOption

  def majorUpdate(c: Version, updates: SortedSet[Version]): Option[Version] =
    updates.filter(v => v.major > c.major).lastOption

  def readUpdates(data: Map[ModuleID, SortedSet[Version]]): List[DependencyUpdate] =
    data
      .map {
        case (m, vs) =>
          val c = Version(m.revision)
          DependencyUpdate(
            moduleName = formatModule(m),
            revision = m.revision,
            patch = patchUpdate(c, vs).map(_.toString),
            minor = minorUpdate(c, vs).map(_.toString),
            major = majorUpdate(c, vs).map(_.toString)
          )
      }
      .toList
      .sortBy(_.moduleName)

}
