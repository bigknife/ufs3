package ufs3.integ



object commons {
  import freestyle._
  import freestyle.implicits._
  import freestyle.effects.error.implicits._
  import _root_.fs2.interop.cats._
  import ufs3.interp.block._
  import ufs3.interp.fildex._
  import ufs3.interp.filler._
  import ufs3.interp.sandwich._
  import ufs3.interp.byteBufferStream._
  import ufs3.kernel.modules._
  import ufs3.interp.commons._
  import ufs3.interp.log._
  import rd.implicits._

  import freestyle.fs2.implicits._
  import ufs3.kernel.commons.Config
  import _root_.fs2.Task
  import ufs3.prog._

  def init(): Unit = {
    // pre create handler, it will take a long time
    implicitly[FSHandler[App.Op, Stack]]
    ()
  }
}