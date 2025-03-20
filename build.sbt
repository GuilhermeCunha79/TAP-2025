name := "pj-2025"

version := "0.1"

scalaVersion := "3.3.5"

scalacOptions ++= Seq("-source:future", "-indent", "-rewrite")

// XML
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.3.0"
// ScalaTest
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
// ScalaCheck
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.18.1" % "test"

wartremoverErrors ++= Warts.allBut(Wart.Any, Wart.Equals, Wart.Nothing,
  Wart.Overloading, Wart.Recursion, Wart.StringPlusAny,
  Wart.ToString, Wart.TripleQuestionMark)
