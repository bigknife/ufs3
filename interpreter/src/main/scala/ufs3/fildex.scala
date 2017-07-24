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

import java.nio.ByteBuffer

import cats.data.Kleisli
import cats.effect.IO
import ufs3.kernel.fildex._
import ufs3.interpreter.block._
import ufs3.interpreter.filler._
import ufs3.interpreter.layout.{FildexFileLayout, FillerFileLayout, IdxLayout}
import ufs3.interpreter.sandwich.Sandwich
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex.{FildexFile, Idx}
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
          val bb    = ff.read(FildexFileLayout.HEAD_SIZE)
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
        val bb    = ff.read(FildexFileLayout.HEAD_SIZE)
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

  def append(bf: FildexFile, idx: Fildex.Idx): Kleisli[IO, FildexInterpreter.Config, FildexFile] = Kleisli { config ⇒
    IO {
      import RandomFildexFile._
      bf.append(idx.key, idx)
    }
  }

  def repair(bf: Block.BlockFile, filler: FillerFile): Kleisli[IO, FildexInterpreter.Config, FildexFile] = Kleisli {
    config ⇒
      IO {
        import RandomAccessBlockFile._
        import RandomFillerFile._
        import RandomFildexFile._
        val underlying = filler.underlying
        def repairIndex(startPos: Long, endPos: Long, fildex: FildexFile): FildexFile = {
          if (endPos > startPos) {
            underlying.seek(startPos + 12)
            val keyArray   = new Array[Byte](32)
            val headBuffer = underlying.read(32)
            headBuffer.get(keyArray)
            val key = new String(keyArray, "utf-8")
            underlying.seek(startPos + Sandwich.HEAD_LENGTH - 8)
            val bodyLengthBuffer = underlying.read(8)
            val bodyLength       = bodyLengthBuffer.getLong
            val end              = startPos + Sandwich.HEAD_LENGTH + bodyLength
            val buffer           = ByteBuffer.allocate(IdxLayout.SIZE.toInt)
            buffer.put(keyArray)
            buffer.putLong(startPos + Sandwich.HEAD_LENGTH)
            buffer.putLong(end)
            val idx       = IdxLayout.resolveBytes(buffer.array())
            val indexFile = fildex.append(key, idx)
            repairIndex(startPos + Sandwich.HEAD_LENGTH + bodyLength + Sandwich.HASH_SIZE + 8, endPos, indexFile)
          } else fildex
        }
        val layout        = FildexFileLayout(bf.size())
        val initIndexFile = RandomFildexFile(layout, bf).init()
        repairIndex(FillerFileLayout.HEAD_SIZE, filler.tailPos, initIndexFile)
      }
  }
}

object FildexInterpreter {
  trait Config {}

  def apply(): FildexInterpreter = new FildexInterpreter {}
}
