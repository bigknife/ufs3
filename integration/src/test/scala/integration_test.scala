package ufs3
package integration
package test

import java.io.{FileInputStream, FileOutputStream, InputStream, OutputStream}
import java.nio.ByteBuffer
import java.security.MessageDigest

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
import ufs3.interpreter.sandwich.{SandwichInInterpreter, SandwichOutInterpreter}
import ufs3.kernel.log.Log
import ufs3.kernel.sandwich.{SandwichIn, SandwichOut}
import ufs3.log.interpreter.LogInterpreter
import ufs3.core.data.Data._
import scala.language.higherKinds
import scala.util.Random

object TestFix {
  type App1[A]       = Coproduct[Block.Op, Filler.Op, A]
  type App2[A]       = Coproduct[Log.Op, App1, A]
  type StartupApp[A] = Coproduct[Fildex.Op, App2, A]

  type WriteApp[A] = Coproduct[SandwichIn.Op[InputStream, ?], StartupApp, A]
  type ReadApp[A]  = Coproduct[SandwichOut.Op[OutputStream, ?], StartupApp, A]

  val bi  = BlockInterpreter()
  val fi  = FillerInterpreter()
  val fii = FildexInterpreter()
  val li  = LogInterpreter()
  val sii = SandwichInInterpreter()
  val soi = SandwichOutInterpreter()

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
  implicit val nt6 = new NT[Kleisli[IO, SandwichOutInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]] {
    def apply[A](fa: Kleisli[IO, SandwichOutInterpreter.Config, A]): Kleisli[IO, UniConfig, A] =
      fa.local[UniConfig](_.sanwichOutConfig)
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
  val sandwichOutInterpreter: NT[SandwichOut.Op[OutputStream, ?], Kleisli[IO, UniConfig, ?]] =
    liftNT[SandwichOut.Op[OutputStream, ?], Kleisli[IO, SandwichOutInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]](
      soi)

  val interpreter
    : NT[StartupApp, Kleisli[IO, UniConfig, ?]] = fildexInterperter or (logInterperter or (blockInterperter or fillerInterperter))

  val writeAppInterpreter: NT[WriteApp, Kleisli[IO, UniConfig, ?]] = sandwichInInterpreter or interpreter

  val readAppInterpreter: NT[ReadApp, Kleisli[IO, UniConfig, ?]] = sandwichOutInterpreter or interpreter

  trait UniConfig {
    def blockConfig: BlockInterpreter.Config   = new BlockInterpreter.Config {}
    def fillerConfig: FillerInterpreter.Config = new FillerInterpreter.Config {}
    def fildexConfig: FildexInterpreter.Config = new FildexInterpreter.Config {}
    def logConfig: LogInterpreter.Config       = LogInterpreter.config()
    def sandwichInConfig: SandwichInInterpreter.Config = new SandwichInInterpreter.Config {
      def inputBufferSize: Int = 8192
    }
    def sanwichOutConfig: SandwichOutInterpreter.Config = new SandwichOutInterpreter.Config {
      def outputBufferSize: Long = 8192
    }
  }

  val coreConfig = new CoreConfig {
    import Block.Size._

    def fillerReadBufferSize: Block.Size = 8.KiB
    val idxBlockSize: Block.Size         = 2.GiB
    val fillerBlockSize: Block.Size      = 100.GiB
    val fillerBlockPath: Block.Path      = Block.Path("/Users/bigknife/Working/tmp/ufs3.filler")
  }

}

object StartupTest {
  import TestFix._
  def test(): Unit = {
    /*
    val start: SOP[StartupApp, UFS3] = startup[StartupApp].run(coreConfig)
    val app = for {
      ufs3 ← start
      _    ← shutdown[StartupApp](ufs3).run(coreConfig)
    } yield ()

    val k = app.foldMap(interpreter)
    k.run(new UniConfig {}).unsafeRunSync()
    println("StartupTest OK")
   */
  }
}

object WriteFileTest {
  import TestFix._
  def test(times: Int): Unit = {
    /*
    val start = System.currentTimeMillis()
    def key(): String   = {
      val dst = Random.nextString(32).getBytes()
      val str = MessageDigest.getInstance("MD5").digest(dst).map("%02x" format _).mkString("")
      str
    }
    def newIns()   = new FileInputStream("/Users/bigknife/Working/tmp/test.jpg")
    val app = for {
      ufs3 ← startup[WriteApp].run(coreConfig)
      allSize ← {
        def run(c: Int): SOP[WriteApp, Long] = {
          if (c <= 0) SOP.pure[WriteApp, Long](0)
          else {
            for {
              ins ← SOP.pure[WriteApp, InputStream](newIns())
              size ← SOP.pure[WriteApp, Int] (ins.available())
              _ ← write[WriteApp, InputStream](key(),  ins, ufs3).run(coreConfig)
              _ ← SOP.pure[WriteApp, Unit]({println(s"writed : $c"); ins.close()})
              writedSize ← run(c - 1)
            } yield size + writedSize
          }
        }
        run(times)
      }
      _ ← shutdown[WriteApp](ufs3).run(coreConfig)
    } yield allSize
    val allSize = app.foldMap(writeAppInterpreter).run(new UniConfig {}).unsafeRunSync()
    val spent = System.currentTimeMillis() - start
    println(s"put $allSize bytes ok. spent: $spent ms, bps: ${allSize / spent.toDouble}")
   */
  }

}

object ReadFileTest {
  import TestFix._
  def test(key: String): Unit = {
    /*
    val out = new FileOutputStream("/Users/bigknife/Working/tmp/test_out.jpg")
    val app = for {
      ufs3 ← startup[ReadApp].run(coreConfig)
      _    ← readWithKey[ReadApp, OutputStream](key, ufs3, out).run(coreConfig)
    } yield ()
    app.foldMap(readAppInterpreter).run(new UniConfig {}).unsafeRunSync()
   */
  }
}

object TestSute {
  def main(args: Array[String]): Unit = {
    // startup , only run once
    //StartupTest.test()
    //println("----------------------")
    // run WriteFileTest.test()
    // test write and read
    //val key = WriteFileTest.test(1)
    WriteFileTest.test(100000)
    //println("--------------")
    //println(key)
    //val key = "4c8611042cca11744dc4c18ca6061be3"
    //val key = "1eaa2913c84c5ee721bb686a25e23726"
    //val key = "acb9ac72b5027d97c1bf83c3c138cc6d"
    //ReadFileTest.test(key)

  }
}
