package dependencies

case class DependencyUpdate(
    moduleName: String,
    revision: String,
    patch: Option[String],
    minor: Option[String],
    major: Option[String])
