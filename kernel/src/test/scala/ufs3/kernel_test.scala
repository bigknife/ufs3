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

import block._
import filler._
import Block.Ops._
import Block._
import cats._
import cats.data.Coproduct
import cats.free._
import ufs3.kernel.block.Block.{Create, Open}
import ufs3.kernel.filler.Filler
import ufs3.kernel.filler.Filler.{InitBlock, ValidateBlock}


object Mock extends App{
  // block interpreter
  def blockInterpreter = new (Block ~> Id) {
    override def apply[A](fa: Block[A]): Id[A] = fa match {
      case Open(path, mode) ⇒
        println(s"open path=${path.file.value}, use mode=$mode")
        if(path.file.value.exists() && path.file.value.isFile) Right(Some(BlockFile(path, mode).value))
        else Right(None)

      case Create(path, size) ⇒
        println(s"create path=${path.file.value}, use mode=$size")
        path.file.value.createNewFile()
        Right(BlockFile(path, FileMode.ReadWrite).value)

      case x ⇒
        println(s"other adt: $x")
        Left(new UnsupportedOperationException(s"$x")).asInstanceOf[A]
    }
  }

  def fillerInterpreter = new (Filler ~> Id) {
    override def apply[A](fa: Filler[A]): Id[A] = fa match {
      case InitBlock(bf) ⇒
        println("filler interpreter: init block")
        Right(())
      case ValidateBlock(bf) ⇒
        println("filler interpreter: validate block")
        Right(())
    }
  }

  type TestApp[A] = Coproduct[Filler, Block, A]
  val interpreter = fillerInterpreter or blockInterpreter


  def program(path: Path,mode: FileMode,size: Size)(implicit F: Filler.Ops[TestApp]): Free[TestApp, Response[Unit]] = {
    import F._
    for {
      a ← openBlock(path, mode, size)
    } yield a
  }
  import Size._
  program(Path("/tmp/test_aaa.txt"), FileMode.ReadWrite, 100.MiB).foldMap(interpreter)
}