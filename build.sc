import mill._
import mill.scalalib._
import coursier.maven.MavenRepository

object outbreak extends ScalaModule {
  def scalaVersion = "2.12.5"
  def repositories = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  )
  def ivyDeps = Agg(
    ivy"com.github.ichoran::thyme:0.1.2-SNAPSHOT"
  )
}
