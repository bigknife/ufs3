package ufs3.prog

import org.scalatest.FlatSpec

class CreateProgramSpec extends FlatSpec {
  "create program" should "create a pair of new filler file and fildex file" in {
    import ufs3.kernel.modules.App
    import cats.implicits._
    import freestyle._

    import ufs3.interp.commons._
    import ufs3.interp.block._
    import ufs3.interp.fildex._
    import ufs3.interp.filler._
    import freestyle.fs2.implicits._
    /*
    import _root_.fs2.interop.cats._

    implicit val _app = App[App.Op]
    */
    create[App.Op]
    //.interpret[Stack]
  }
}