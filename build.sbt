organization  := "shorten"

version       := "0.1"

scalaVersion  := "2.11.2"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val akkaV = "2.3.6"
  val sprayV = "1.3.2"
  Seq(
    "io.spray"          %% "spray-can"          % sprayV,
    "io.spray"          %% "spray-routing"      % sprayV,
    "io.spray"          %% "spray-json"         % "1.3.1",
    "io.spray"          %% "spray-testkit"      % sprayV    % "test",
    "com.typesafe.akka" %% "akka-actor"         % akkaV,
    "com.typesafe.akka" %% "akka-testkit"       % akkaV     % "test",
    "org.specs2"        %% "specs2-core"        % "2.3.11"  % "test",
    "org.scalikejdbc"   %% "scalikejdbc"        % "2.2.0",
    "org.scalikejdbc"   %% "scalikejdbc-config" % "2.2.0",
    "org.scalikejdbc"   %% "scalikejdbc-test"   % "2.2.0"   % "test",
    "org.scalikejdbc"   %% "scalikejdbc-syntax-support-macro" % "2.2.0",
    "org.postgresql"    %  "postgresql"         % "9.3-1100-jdbc41",
    "ch.qos.logback"    %  "logback-classic"    % "1.1.2"
  )
}

Revolver.settings

seq(flywaySettings: _*)

flywayDriver := "org.postgresql.Driver"

flywayUrl := "jdbc:postgresql://localhost:5432/shorten"

flywayUser := "cvb"

flywayPassword := "qwe"
