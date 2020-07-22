name := "withCats"

version := "0.1"

scalaVersion := "2.13.2"

scalacOptions += {
  "-language:higherKinds"
}

val catsVersion       = "2.1.1"

libraryDependencies += "org.typelevel" %% "cats-core"   % catsVersion
libraryDependencies += "org.typelevel" %% "cats-free"   % catsVersion

addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1")
addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.11.0" cross CrossVersion.patch)
