import sbt._
import Keys._

object Build extends Build {

  // version of virtualized scala that is used
  val virtScala = "2.10.2"

  // The OptiREScala project
  lazy val root = Project(id = "root", base = file(".")).settings(
    name := "OptiREScala",

    scalacOptions ++= Seq( "-deprecation",
      "-unchecked",
      "-feature",
      "-optimise",
      "-Yinline-warnings",
      "-Yvirtualize",
      "-Xexperimental"),

    scalaVersion := "2.10.2",

    // required for -Yvirtualize:
    scalaOrganization := "org.scala-lang.virtualized",

    libraryDependencies ++= Seq( "org.scalatest" %% "scalatest" % "2.1.2"
      , "EPFL" %% "lms" % "0.3-SNAPSHOT"
      , "org.scala-lang.virtualized" % "scala-compiler" % virtScala
      , "org.scala-lang.virtualized" % "scala-library" % virtScala
      , "org.scala-lang" % "scala-actors" % virtScala
    )

  ).dependsOn(
    // Depend on REScala repository
    RootProject(file("project/REScala-clone"))
  )

}
