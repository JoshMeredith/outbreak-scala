import mill._
import mill.scalalib._
import coursier.maven.MavenRepository

def scalaVer = "2.12.8"

object outbreak extends ScalaModule {
  def scalaVersion = scalaVer
  def moduleDeps = Seq(outbreakDatabase)
  def repositories = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  )
  def ivyDeps = Agg(
    ivy"com.github.ichoran::thyme:0.1.2-SNAPSHOT"
  )
}

object outbreakDatabase extends ScalaModule {
  def scalaVersion = scalaVer
  def repositories = outbreak.repositories
  def ivyDeps = Agg(
    ivy"com.typesafe.slick::slick:3.3.0",
    ivy"org.slf4j:slf4j-nop:1.6.4",
    ivy"com.typesafe.slick::slick-hikaricp:3.2.3",
    ivy"com.nrinaudo::kantan.csv:0.4.0",
    ivy"org.xerial:sqlite-jdbc:3.7.2"
  )
}

object importData extends ScalaModule {
  def scalaVersion = scalaVer
  def moduleDeps = Seq(outbreakDatabase)
  def repositories = outbreakDatabase.repositories
  def ivyDeps = outbreakDatabase.ivyDeps
}
