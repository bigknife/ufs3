/**
  * kernel_test.scala
  * ----------
  * kernel test cases
  * @author bigknife
  * @since 2017/7/6
  */
package ufs3
package kernel
package test

import java.io.{FileInputStream, FileOutputStream, InputStream}
import java.nio.ByteBuffer

import block._
import filler._
import cats._
import cats.data.Coproduct
import cats.free._
import org.scalatest.FlatSpec
import ufs3.kernel.block.Block.{Create, Open}
import ufs3.kernel.filler.Filler
import sop._

object SandwitchTest {
  import sandwich._
  import cats.effect.IO
  def interpreter(callback: ByteBuffer ⇒ Unit): Sandwich.Handler[IO, InputStream] = {
    new Sandwich.Handler[IO, InputStream] {
      def head(): IO[ByteBuffer] = IO { val buf = ByteBuffer.wrap("---HEAD--\r\n".getBytes); callback(buf); buf }
      def nextBody(in: InputStream): IO[Option[ByteBuffer]] = {
        val buffer = new Array[Byte](8 * 1024)
        val readed = in.read(buffer)
        if (readed == -1) IO { None } else
          IO {
            val bb = ByteBuffer.wrap(buffer, 0, readed)
            callback(bb)
            Some(bb)
          }
      }
      def tail(): IO[ByteBuffer] = IO { val buf = ByteBuffer.wrap("---TAIL---\r\n".getBytes); callback(buf); buf }
    }
  }

  def test(): Unit = {
    val sandwith = Sandwich[Sandwich.Op[InputStream, ?], InputStream]
    import sandwith._
    def allBody(ins: InputStream): SOP[Sandwich.Op[InputStream, ?], Unit] =
      for {
        obb ← nextBody(ins)
        _ ← if (obb.nonEmpty) allBody(ins)
        else Par.pure[Sandwich.Op[InputStream, ?], Unit](()): SOP[Sandwich.Op[InputStream, ?], Unit]
      } yield ()
    def program(in: InputStream) =
      for {
        _ ← head()
        _ ← allBody(in)
        _ ← tail()
      } yield ()

    /*
    val ins = new FileInputStream("/tmp/a.txt")
    val output = new FileOutputStream("/tmp/c.txt")
    program(ins).foldMap(interpreter{x ⇒ output.write(x.array())}).unsafeRunSync()
     */

    def copy(from: String, to: String) = {
      val insIO = IO {
        new FileInputStream(from)
      }
      val outIO = IO {
        new FileOutputStream(to)
      }
      for {
        ins1 ← insIO
        out1 ← outIO
        _ ← program(ins1).foldMap(interpreter { x ⇒
          println(s"writing ins1 ${x.array().length}")
          out1.write(x.array())
        })
        ins2 ← insIO
        _ ← program(ins2).foldMap(interpreter { x ⇒
          println(s"writing ins2 ${x.array().length}")
          out1.write(x.array())
        })
        _ ← IO {
          ins1.close(); ins2.close(); out1.close()
        }
      } yield ()
    }

    copy("/tmp/a.txt", "/tmp/d.txt").unsafeRunSync()
  }
}

class KernelSpec extends FlatSpec {
  "Sandwich" should "streaming R/W" in {
    SandwitchTest.test()
  }

  // block interpreter
  /*
  def blockInterpreter = new NT[Block, Id] {
    override def apply[A](fa: Block[A]): Id[A] = fa match {
      case Open(path, mode) ⇒
        println(s"${Thread.currentThread()} open path=${path.file.value}, use mode=$mode")
        if(path.file.value.exists() && path.file.value.isFile) Right(Some(BlockFile(path, mode).value))
        else Right(None)

      case Create(path, size) ⇒
        println(s"${Thread.currentThread()} create path=${path.file.value}, use mode=$size")
        path.file.value.createNewFile()
        Right(BlockFile(path, FileMode.ReadWrite).value)

      case x ⇒
        println(s"${Thread.currentThread()} other adt: $x")
        Left(new UnsupportedOperationException(s"$x")).asInstanceOf[A]
    }
  }
   */
  /*
  def fillerInterpreter = new (Filler ~> Id) {
    override def apply[A](fa: Filler[A]): Id[A] = fa match {
      case InitBlock(bf) ⇒
        println(s"${Thread.currentThread()} filler interpreter: init block")
        Right(FillerFile())
      case ValidateBlock(bf) ⇒
        println(s"${Thread.currentThread()} filler interpreter: validate block")
        Right(FillerFile())
    }
  }

  type TestApp[A] = Coproduct[Filler, Block, A]
  val interpreter = fillerInterpreter or blockInterpreter


  def program(path: Path,mode: FileMode,size: Size)(implicit F: Filler.Ops[TestApp]): Free[TestApp, Response[FillerFile]] = {
    import F._
    for {
      a ← openFillerFile(path, mode, size)
    } yield a
  }
  import Size._
  program(Path("/tmp/test_aaa.txt"), FileMode.ReadWrite, 100.MiB).foldMap(interpreter)
 */
}
