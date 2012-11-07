/** Project */
name := "tictactoe"

version := "1.0"

organization := "org.specs2"

scalaVersion := "2.9.2"

/** Shell */
shellPrompt := { state => System.getProperty("user.name") + "> " }

shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

/** Dependencies */
resolvers ++= Seq("releases" at "http://oss.sonatype.org/content/repositories/releases",
                  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")

libraryDependencies ++= Seq("org.specs2" %% "specs2" % "1.12.3-SNAPSHOT",
                            "org.scalacheck" %% "scalacheck" % "1.9")

logBuffered := false

cancelable := true

/** Console */
initialCommands in console := "import org.specs2._"
