import sbt._
import Keys._

object Build extends Build {

  // version of virtualized scala that is used
  val virtScala = "2.10.2"

  // The OptiREScala project
  lazy val root = Project(id = "root", base = file(".")).settings(
    name := "OptiREScala",

    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked",
      "-feature",
      "-optimise",
      "-Yinline-warnings",
      "-Yvirtualize",
      "-Xexperimental"),

    scalaVersion := "2.10.2",

    // required for -Yvirtualize:
    scalaOrganization := "org.scala-lang.virtualized",

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-swing" % "2.10.4",
      "org.scalatest" %% "scalatest" % "2.1.2",
      "org.scala-lang.virtualized" % "scala-compiler" % virtScala,
      "org.scala-lang.virtualized" % "scala-library" % virtScala,
      "org.scala-lang" % "scala-actors" % virtScala
    )

  ).dependsOn(
    RootProject(file("dependencies/REScala"))
  ).dependsOn(
    RootProject(file("dependencies/virtualization-lms-core"))
  )
}
