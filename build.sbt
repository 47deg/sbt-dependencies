pgpPassphrase := Some(getEnvVar("PGP_PASSPHRASE").getOrElse("").toCharArray)
pgpPublicRing := file(s"$gpgFolder/pubring.gpg")
pgpSecretRing := file(s"$gpgFolder/secring.gpg")

lazy val pluginSettings = Seq(
    name := "sbt-dependencies",
    sbtPlugin := true,
    scalaVersion := "2.10.6",
    crossScalaVersions := Seq("2.10.6"),
    scalaOrganization := "org.scala-lang",
    libraryDependencies ++= Seq(
      %%("github4s"),
      %%("scalatest")  % "test",
      %%("scalacheck") % "test"
    )
  )

lazy val `sbt-dependencies` = (project in file("."))
  .settings(pluginSettings: _*)
  .settings(addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.0"))
