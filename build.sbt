inThisBuild(
  List(
    organization := "io.github.pityka",
    homepage := Some(url("https://pityka.github.io/pairwisealignment/")),
    licenses := List(
      ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0"))
    ),
    developers := List(
      Developer(
        "pityka",
        "Istvan Bartha",
        "bartha.pityu@gmail.com",
        url("https://github.com/pityka/pairwisealignment")
      )
    )
  )
)

ThisBuild / versionScheme := Some("early-semver")

ThisBuild / versionPolicyIntention := Compatibility.BinaryAndSourceCompatible
ThisBuild / versionPolicyIgnoredInternalDependencyVersions := Some(
  "^\\d+\\.\\d+\\.\\d+\\+\\d+".r
)

lazy val githubPackagesSettings: Seq[Def.Setting[_]] =
  if (sys.env.get("GITHUB_PACKAGES_PUBLISH").exists(_.nonEmpty))
    Seq(
      publishTo := Some(
        "GitHub Packages" at "https://maven.pkg.github.com/pityka/pairwisealignment"
      ),
      credentials += Credentials(
        "GitHub Package Registry",
        "maven.pkg.github.com",
        sys.env.getOrElse("GITHUB_ACTOR", "pityka"),
        sys.env.getOrElse("GITHUB_TOKEN", "")
      )
    )
  else Seq.empty

lazy val gitlabPackagesSettings: Seq[Def.Setting[_]] =
  (
    sys.env.get("GITLAB_DEPLOY_TOKEN"),
    sys.env.get("GITLAB_DEPLOY_TOKEN_USER"),
    sys.env.get("GITLAB_PROJECT_ID")
  ) match {
    case (Some(token), Some(user), Some(projectId))
        if token.nonEmpty && user.nonEmpty && projectId.nonEmpty =>
      Seq(
        publishTo := Some(
          "gitlab" at s"https://gitlab.com/api/v4/projects/$projectId/packages/maven"
        ),
        credentials += Credentials(
          "GitLab Packages Registry",
          "gitlab.com",
          user,
          token
        )
      )
    case _ => Seq.empty
  }

lazy val commonSettings = Seq(
  scalaVersion := "2.13.18",
  crossScalaVersions := Seq("2.13.18", "3.3.8"),
  parallelExecution in Test := false,
  mimaPreviousArtifacts := (CrossVersion.partialVersion(
    scalaVersion.value
  ) match {
    case Some((2, _)) => Set(organization.value %% moduleName.value % "2.2.3")
    case Some((3, _)) => Set()
    case _            => ???
  }),
  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) =>
      Seq(
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-encoding",
        "utf-8", // Specify character encoding used by source files.
        "-feature", // Emit warning and location for usages of features that should be imported explicitly.
        "-language:postfixOps",
        "-language:existentials",
        "-unchecked", // Enable additional warnings where generated code depends on assumptions.
        "-Xfatal-warnings" // Fail the compilation if there are any warnings.
      )
    case Some((2, _)) =>
      Seq(
        "-opt:l:method",
        "-opt:l:inline",
        "-opt-inline-from:org.saddle.**",
        "-opt-warnings",
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-encoding",
        "utf-8", // Specify character encoding used by source files.
        "-feature", // Emit warning and location for usages of features that should be imported explicitly.
        "-language:postfixOps",
        "-language:existentials",
        "-unchecked", // Enable additional warnings where generated code depends on assumptions.
        "-Xfatal-warnings", // Fail the compilation if there are any warnings.
        "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
        "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
        "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
        "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
        "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
        "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
        "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
        "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
        "-Xlint:option-implicit", // Option.apply used implicit view.
        "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
        "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
        "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
        "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
        // "-Ywarn-dead-code", // Warn when dead code is identified.
        // "-Ywarn-numeric-widen", // Warn when numerics are widened.
        "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
        "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
        "-Ywarn-unused:locals", // Warn if a local definition is unused.
        "-Ywarn-unused:params", // Warn if a value parameter is unused.
        "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
        "-Ywarn-unused:privates" // Warn if a private member is unused.
      )
    case _ => ???
  }),
  scalacOptions in (Compile, console) ~= (_ filterNot (_ == "-Xfatal-warnings"))
) ++ Seq(
  fork := false,
  cancelable in Global := true
) ++ githubPackagesSettings ++ gitlabPackagesSettings

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure) 
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "pairwisealignment",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.3.6" % Test
    )
  )

lazy val root = project
  .in(file("."))
  .aggregate(core.js,core.jvm,core.native)
  .settings(
    scalaVersion := "2.13.18",
    crossScalaVersions := Nil,
    publish := {},
    publishLocal := {},
    publishArtifact := false
  )
