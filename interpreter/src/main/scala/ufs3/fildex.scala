/**
  * fildex.scala
  * ----------
  * interpreter of Fildex
  * @author bigknife
  * @since 2017/7/18
  */
package ufs3
package interpreter
package fildex

import cats.data.Kleisli
import cats.effect.IO
import ufs3.kernel.fildex._
import ufs3.interpreter.block._
import ufs3.interpreter.filler._
import ufs3.interpreter.layout.FildexFileLayout
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex.FildexFile
import ufs3.kernel.filler.Filler.FillerFile

trait FildexInterpreter extends Fildex.Handler[Kleisli[IO, FildexInterpreter.Config, ?]] {
  import RandomAccessBlockFile._
  def init(ff: Block.BlockFile): Kleisli[IO, FildexInterpreter.Config, Fildex.FildexFile] = Kleisli { config ⇒
    IO {
      import RandomAccessBlockFile._
      val layout = FildexFileLayout(ff.size())
      RandomFildexFile(layout, ff).init()
    }
  }

  def close(ff: Fildex.FildexFile): Kleisli[IO, FildexInterpreter.Config, Unit] =
    Kleisli.pure[IO, FildexInterpreter.Config, Unit](())

  def check(ff: Block.BlockFile, filler: FillerFile): Kleisli[IO, FildexInterpreter.Config, Boolean] = Kleisli {
    config ⇒
      IO {
        // check the blockFile size if gt index file head
        require(ff.size() >= FildexFileLayout.HEAD_SIZE,
                s"fildex file length should greater than ${FildexFileLayout.HEAD_SIZE}")

        val headBytes = {
          ff.seek(0)
          val bb = ff.read(FildexFileLayout.HEAD_SIZE)
          val bytes = new Array[Byte](FildexFileLayout.HEAD_SIZE.toInt)
          bb.get(bytes)
          bytes
        }

        val layout = FildexFileLayout.resolveBytes(headBytes)

        // check the version is the same
        import RandomFillerFile._
        layout.version.intValue == (filler: RandomFillerFile).version
      }
  }

  def load(ff: Block.BlockFile): Kleisli[IO, FildexInterpreter.Config, Fildex.FildexFile] = Kleisli { config ⇒
    IO {
      val headBytes = {
        ff.seek(0)
        val bb = ff.read(FildexFileLayout.HEAD_SIZE)
        val bytes = new Array[Byte](FildexFileLayout.HEAD_SIZE.toInt)
        bb.get(bytes)
        bytes
      }
      val layout = FildexFileLayout.resolveBytes(headBytes)
      RandomFildexFile(layout, ff).loadIndex()
    }
  }

  def fetch(key: String, fildex: FildexFile): Kleisli[IO, FildexInterpreter.Config, Option[Fildex.Idx]] = Kleisli {
    config ⇒
      IO {
        import RandomFildexFile._
        fildex.fetchIdx(key)
      }
  }

  def append(bf: FildexFile, idx: Fildex.Idx): Kleisli[IO, FildexInterpreter.Config, FildexFile] = Kleisli {config ⇒
    IO {
      import RandomFildexFile._
      bf.append(idx.key, idx)
    }
  }


  def repair(bf: Block.BlockFile, filler: FillerFile): Kleisli[IO, FildexInterpreter.Config, FildexFile] = Kleisli {config ⇒
    IO {
      //TODO repair
      //
      ???
    }
  }
}

object FildexInterpreter {
  trait Config {}

  def apply(): FildexInterpreter = new FildexInterpreter {}
}
