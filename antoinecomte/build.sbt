name:= "lambda calculus workshop"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

import scalariform.formatter.preferences._
scalariformSettings

ScalariformKeys.preferences := FormattingPreferences()
  .setPreference(AlignParameters, true)
  .setPreference(RewriteArrowSymbols, true)
  .setPreference(PreserveDanglingCloseParenthesis, false)

