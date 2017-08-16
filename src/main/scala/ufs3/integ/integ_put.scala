package ufs3.integ

import java.io.InputStream

import ufs3.kernel.commons.FileMode

object put {
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
  import rd.implicits._
  import freestyle.loggingJVM.implicits._
  import freestyle.fs2.implicits._
  import ufs3.kernel.commons.Config
  import _root_.fs2.Task
  import ufs3.prog._

  def apply(config: Config, key: String, in: InputStream): Task[Unit] = {
    //ufs3.prog.create[App.Op].interpret[Stack].run(config)
    println(s"start: ${System.currentTimeMillis()}")
    val p: FreeS[App.Op, Unit] = for {
      ufs3 ← open[App.Op](FileMode.ReadWrite)
      _    ← write[App.Op](key, in, ufs3)
    } yield ()
    println(s"create p: ${System.currentTimeMillis()}")
    val ops = new FreeSOps[App.Op, Unit](p)
    println(s"create ops: ${System.currentTimeMillis()}")
    val f = ops.interpret[Stack]
    println(s"interpret: ${System.currentTimeMillis()}")
    val s = f.run(config)
    println(s"run: ${System.currentTimeMillis()}")
    s
  }
}
