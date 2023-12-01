import mill._, scalalib._

object root extends RootModule with ScalaModule {
  def scalaVersion = "3.2.2"

  object test extends ScalaTests with TestModule.Munit {
    def ivyDeps =
      Agg(
        ivy"org.scalameta::munit::0.7.29",
        ivy"org.scala-lang::toolkit-test:0.1.7",
        ivy"org.scala-lang::toolkit:0.1.7"
      )
  }
}
