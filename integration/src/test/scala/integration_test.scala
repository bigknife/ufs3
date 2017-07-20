package ufs3
package integration
package test

import java.io.{FileInputStream, InputStream}

import cats.Id
import cats.data.{Coproduct, Kleisli}
import cats.effect.IO
import cats.free.Inject
import sop.{NT, Par, SOP}
import ufs3.core._
import ufs3.interpreter.block.BlockInterpreter
import ufs3.interpreter.fildex.FildexInterpreter
import ufs3.interpreter.filler.FillerInterpreter
import ufs3.kernel.block._
import ufs3.kernel.fildex._
import ufs3.kernel.filler._
import sop._
import ufs3.interpreter.sandwich.SandwichInInterpreter
import ufs3.kernel.log.Log
import ufs3.kernel.sandwich.SandwichIn
import ufs3.log.interpreter.LogInterpreter

import scala.language.higherKinds

object TestFix {
  type App1[A]       = Coproduct[Block.Op, Filler.Op, A]
  type App2[A]       = Coproduct[Log.Op, App1, A]
  type StartupApp[A] = Coproduct[Fildex.Op, App2, A]

  type WriteApp[A] = Coproduct[SandwichIn.Op[InputStream, ?], StartupApp, A]

  val bi  = BlockInterpreter()
  val fi  = FillerInterpreter()
  val fii = FildexInterpreter()
  val li  = LogInterpreter()
  val sii = SandwichInInterpreter()
  // NT → NT
  def liftNT[F[_], G[_], H[_]](nt: NT[F, G])(implicit nt1: NT[G, H]): NT[F, H] = nt1 compose nt

  implicit val nt1 = new NT[Kleisli[IO, BlockInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]] {
    def apply[A](fa: Kleisli[IO, BlockInterpreter.Config, A]): Kleisli[IO, UniConfig, A] =
      fa.local[UniConfig](_.blockConfig)
  }
  implicit val nt2 = new NT[Kleisli[IO, FillerInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]] {
    def apply[A](fa: Kleisli[IO, FillerInterpreter.Config, A]): Kleisli[IO, UniConfig, A] =
      fa.local[UniConfig](_.fillerConfig)
  }
  implicit val nt3 = new NT[Kleisli[IO, FildexInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]] {
    def apply[A](fa: Kleisli[IO, FildexInterpreter.Config, A]): Kleisli[IO, UniConfig, A] =
      fa.local[UniConfig](_.fildexConfig)
  }
  implicit val nt4 = new NT[Kleisli[IO, LogInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]] {
    def apply[A](fa: Kleisli[IO, LogInterpreter.Config, A]): Kleisli[IO, UniConfig, A] =
      fa.local[UniConfig](_.logConfig)
  }
  implicit val nt5 = new NT[Kleisli[IO, SandwichInInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]] {
    def apply[A](fa: Kleisli[IO, SandwichInInterpreter.Config, A]): Kleisli[IO, UniConfig, A] =
      fa.local[UniConfig](_.sandwichInConfig)
  }

  val blockInterperter: NT[Block.Op, Kleisli[IO, UniConfig, ?]] =
    liftNT[Block.Op, Kleisli[IO, BlockInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]](bi)
  val fillerInterperter: NT[Filler.Op, Kleisli[IO, UniConfig, ?]] =
    liftNT[Filler.Op, Kleisli[IO, FillerInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]](fi)
  val fildexInterperter: NT[Fildex.Op, Kleisli[IO, UniConfig, ?]] =
    liftNT[Fildex.Op, Kleisli[IO, FildexInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]](fii)
  val logInterperter: NT[Log.Op, Kleisli[IO, UniConfig, ?]] =
    liftNT[Log.Op, Kleisli[IO, LogInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]](li)
  val sandwichInInterpreter: NT[SandwichIn.Op[InputStream, ?], Kleisli[IO, UniConfig, ?]] =
    liftNT[SandwichIn.Op[InputStream, ?], Kleisli[IO, SandwichInInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]](sii)

  val interpreter
    : NT[StartupApp, Kleisli[IO, UniConfig, ?]] = fildexInterperter or (logInterperter or (blockInterperter or fillerInterperter))

  val writeAppInterpreter: NT[WriteApp, Kleisli[IO, UniConfig, ?]] = sandwichInInterpreter or interpreter

  trait UniConfig {
    def blockConfig: BlockInterpreter.Config   = new BlockInterpreter.Config {}
    def fillerConfig: FillerInterpreter.Config = new FillerInterpreter.Config {}
    def fildexConfig: FildexInterpreter.Config = new FildexInterpreter.Config {}
    def logConfig: LogInterpreter.Config       = LogInterpreter.config()
    def sandwichInConfig: SandwichInInterpreter.Config = new SandwichInInterpreter.Config {
      def inputBufferSize: Int = 8192
    }
  }

  val coreConfig = new CoreConfig {
    import Block.Size._

    val idxBlockSize: Block.Size    = 2.GiB
    val fillerBlockSize: Block.Size = 100.GiB
    val fillerBlockPath: Block.Path = Block.Path("/Users/bigknife/Working/tmp/ufs3.filler")
  }

}

object StartupTest {
  import TestFix._
  def test(): Unit = {

    val start: SOP[StartupApp, UFS3] = startup[StartupApp].run(coreConfig)
    val app = for {
      ufs3 ← start
      _    ← shutdown[StartupApp](ufs3).run(coreConfig)
    } yield ()

    val k = app.foldMap(interpreter)
    k.run(new UniConfig {}).unsafeRunSync()
    println("StartupTest OK")
  }
}

object WriteFileTest {
  import TestFix._
  def test(): Unit = {

    val key = "12345678901234567890123456789012"
    val ins = new FileInputStream("/Users/bigknife/Working/tmp/test.jpg")
    val app = for {
      ufs3 ← startup[WriteApp].run(coreConfig)
      _    ← write[WriteApp, InputStream](key, ins.available().toLong, ins, ufs3).run(coreConfig)
      _    ← shutdown[WriteApp](ufs3).run(coreConfig)
    } yield ()
    app.foldMap(writeAppInterpreter).run(new UniConfig {}).unsafeRunSync()
    println("WriteFileTest OK")
  }
}

object TestSute {
  def main(args: Array[String]): Unit = {
    StartupTest.test()
    println("----------------------")
    WriteFileTest.test()
  }
}
