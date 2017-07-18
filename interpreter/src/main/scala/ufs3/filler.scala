/**
  * filler.scala
  * ----------
  * interpreter of filler
  * @author bigknife
  * @since 2017/7/18
  */
package ufs3
package interpreter
package filler

import java.nio.ByteBuffer

import cats.data.Kleisli
import ufs3.kernel.filler._
import cats.effect.IO
import ufs3.kernel.block.Block
import ufs3.interpreter.block.RandomAccessBlockFile


/**
  * Filler Interpreter
  * Filler File Layout:
  * [0, 4): head magic 'fill'
  * [4, 12): blocksize
  * [12, 20): tail position
  */
trait FillerInterpreter extends Filler.Handler[Kleisli[IO, FillerInterpreter.Config, ?]] {

  def init(blockFile: Block.BlockFile): Kleisli[IO, FillerInterpreter.Config, Filler.FillerFile] = Kleisli { config ⇒
    // init: seek to 0, and insert filler head bytes
    IO {
      import RandomAccessBlockFile._
      val rff = RandomFillerFile(tailPosition = 0, underlying = blockFile)
      blockFile.seek(0)
      blockFile.write(rff.headBytes, RandomFillerFile.HEAD_SIZE)
      rff
    }
  }

  def close(ff: Filler.FillerFile): Kleisli[IO, FillerInterpreter.Config, Unit] = Kleisli { config ⇒
    IO {
      // now nothing to do here
    }
  }

  def check(blockFile: Block.BlockFile): Kleisli[IO, FillerInterpreter.Config, Filler.FillerFile] = Kleisli { config ⇒
    IO {
      // if first 4bytes is the magic
      import RandomAccessBlockFile._
      if (blockFile.size() < RandomFillerFile.HEAD_SIZE) throw new IllegalAccessException("the block file is not a FillerFile")

      val headBytes = {
        blockFile.seek(0); blockFile.read(RandomFillerFile.HEAD_SIZE)
      }.array()
      val (magic, rest1) = headBytes.splitAt(4)


      if (!RandomFillerFile.magicMatched(magic)) throw new IllegalAccessException("the block file is not a FillerFile")
      else {
        val (blockSize, tailPosition) = rest1.splitAt(8)

        def bytesToLong(bytes: Array[Byte]): Long = {
          val bf = ByteBuffer.wrap(bytes)
          bf.flip()
          bf.getLong
        }

        // blockSize === blockFile.size
        if (bytesToLong(blockSize) != blockFile.size()) throw new IllegalStateException("the filler file block size is not illegal")

        RandomFillerFile(tailPosition = bytesToLong(tailPosition), underlying = blockFile)
      }
    }
  }

  def startAppend(ff: Filler.FillerFile): Kleisli[IO, FillerInterpreter.Config, Long] = Kleisli { config ⇒
    IO {
      import RandomFillerFile._
      ff.tailPos
    }
  }

  def endAppend(ff: Filler.FillerFile, endPosition: Long): Kleisli[IO, FillerInterpreter.Config, Unit] = Kleisli {config ⇒
    IO {
      import RandomFillerFile._
      ff.tailMoveTo(endPosition)
    }
  }
}

object FillerInterpreter {
  trait Config {}

  def apply(): FillerInterpreter = new FillerInterpreter(){}
}