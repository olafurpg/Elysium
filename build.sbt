name := "Inline Macros"

lazy val commonSettings = Seq(
  version := "1.0",
  scalaVersion := "2.11.8",
  scalacOptions += "-Xplugin-require:macroparadise",
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  addCompilerPlugin("org.scalamacros" % "paradise" % "3.0.0-M3" cross CrossVersion.full),
  libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.0-RC4"
)

lazy val root = (project in file("."))
    .settings(commonSettings:_*)
    .dependsOn(macros)

lazy val macros = (project in file("macros"))
    .settings(commonSettings:_*)
    .settings(
      libraryDependencies += "org.scalameta" %% "scalameta" % "1.0.0"
)
