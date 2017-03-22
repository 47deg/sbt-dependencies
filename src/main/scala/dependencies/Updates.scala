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
    updates.filter { v =>
      v.major == c.major && v.minor == c.minor
    }.lastOption

  def minorUpdate(c: Version, updates: SortedSet[Version]): Option[Version] =
    updates.filter { v =>
      v.major == c.major && v.minor > c.minor
    }.lastOption

  def majorUpdate(c: Version, updates: SortedSet[Version]): Option[Version] =
    updates.filter { v =>
      v.major > c.major
    }.lastOption

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
