name := "sangria-playground"
description := "An example of GraphQL server written with Play and Sangria."

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.sangria-graphql" %% "sangria" % "0.4.1",
  // WebJars (i.e. client-side) dependencies
  "org.webjars" % "react" % "0.13.3",
  "org.webjars.bower" % "fetch" % "0.9.0",
  "org.webjars" % "jquery" % "2.1.4"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

routesGenerator := InjectedRoutesGenerator

lazy val root = (project in file(".")).enablePlugins(PlayScala)

herokuAppName in Compile := "aqueous-hollows-6102"
herokuConfigVars in Compile := Map(
  "JAVA_OPTS" -> "-DgaCode=UA-65759630-2 -DdefaultGraphQLUrl=http://try.sangria-graphql.org/graphql"
)

// Apply digest calculation and gzip compression to assets
pipelineStages := Seq(digest, gzip)
JsEngineKeys.engineType := JsEngineKeys.EngineType.Node