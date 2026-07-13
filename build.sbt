ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.1"

name := "PPM_Projeto"

libraryDependencies ++= Seq(
  "org.scalafx" %% "scalafx" % "21.0.0-R32",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"
)

// Necessário para o JavaFX funcionar
val javafxModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

libraryDependencies ++= javafxModules.map(m =>
  "org.openjfx" % s"javafx-$m" % "21" classifier osName
)