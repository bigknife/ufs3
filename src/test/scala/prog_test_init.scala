package ufs3.prog

import cats.Monad
import _root_.fs2.Task
import org.scalatest.FlatSpec
import ufs3.kernel.commons.{Config, Path}

import scala.language.postfixOps

class CreateProgramSpec extends FlatSpec {
  "create program" should "create a pair of new filler file and fildex file" in {
    /*
    import freestyle._
    import freestyle.implicits._
    import freestyle.effects.error.implicits._
    import _root_.fs2.interop.cats._
    import ufs3.interp.block._
    import ufs3.interp.fildex._
    import ufs3.interp.filler._
    import ufs3.kernel.modules._
    import ufs3.interp.commons._
    import rd.implicits._
    import freestyle.loggingJVM.implicits._


    val s = create[App.Op].interpret[Stack]
    import ufs3.kernel.commons.Size._
    val config = Config(fillerBlockPath = Path("/tmp/a.filler"),
      fillerBlockSize = 1 GiB,
      idxBlockSize = 10 MiB,
      fillerReadBufferSize = 8 KiB
    )
    val task: Task[Unit] = s.run(config)
    task.unsafeRun()
    */
    import ufs3.kernel.commons.Size._
    val config = Config(fillerBlockPath = Path("/tmp/a.filler"),
      fillerBlockSize = 1 GiB,
      idxBlockSize = 10 MiB,
      fillerReadBufferSize = 8 KiB
    )
    import ufs3.integ._
    create(config).unsafeRun()

  }
}