organization  := "se.ramn"

version       := "0.1"

scalaVersion  := "2.10.1"

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-encoding", "utf8",
  "-optimise"
)

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "1.9" % "test"
)
