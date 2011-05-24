import sbt._

class ConfiggyProject(info: ProjectInfo) extends DefaultProject(info) {
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.8" % "test"

  override def pomExtra =
    (<name>Configgy</name>
    <description>Configgy logging removed</description>
    <url>http://github.com/derekjw/configgy</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>)
}
