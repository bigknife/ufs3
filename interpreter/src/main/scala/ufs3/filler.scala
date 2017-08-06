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

import java.util.concurrent.atomic.AtomicInteger

import cats.data.Kleisli
import ufs3.kernel.filler._
import cats.effect.IO
import sop.Resp
import ufs3.kernel.block.Block
import ufs3.interpreter.block.RandomAccessBlockFile
import ufs3.interpreter.layout.FillerFileLayout
import ufs3.kernel.filler.Filler.FillerFile

/**
  * Filler Interpreter
  * Filler File Layout: @see {FillerFileLayout}
  */
trait FillerInterpreter extends Filler.Handler[Kleisli[IO, FillerInterpreter.Config, ?]] {

  def init(blockFile: Block.BlockFile): Kleisli[IO, FillerInterpreter.Config, Resp[Filler.FillerFile]] = Kleisli {
    config ⇒
      // init: seek to 0, and insert filler head bytes
      IO {
        try {
          import RandomAccessBlockFile._
          val layout = FillerFileLayout(blockFile.size())
          Right(RandomFillerFile(layout, underlying = blockFile).init())
        } catch {
          case t: Throwable ⇒ Left(t)
        }

      }
  }

  def close(ff: Filler.FillerFile): Kleisli[IO, FillerInterpreter.Config, Resp[Unit]] = Kleisli { config ⇒
    IO {
      // now nothing to do here
      Right(())
    }
  }

  def check(blockFile: Block.BlockFile): Kleisli[IO, FillerInterpreter.Config, Resp[Filler.FillerFile]] = Kleisli {
    config ⇒
      IO {
        // 0. check the size if >= head size
        // 1. read all head
        // 2. check if magic eq the 'FILL'
        // 3. check if blockSize eq blockFile.size
        try {
          import RandomAccessBlockFile._
          require(blockFile.size() >= FillerFileLayout.HEAD_SIZE,
                  s"the block file length should be greater than ${FillerFileLayout.HEAD_SIZE}")

          val headBytes = {
            blockFile.seek(0)
            val bb    = blockFile.read(FillerFileLayout.HEAD_SIZE)
            val bytes = new Array[Byte](FillerFileLayout.HEAD_SIZE.toInt)
            bb.get(bytes)
            bytes
          }
          // the magic check is in the `resoveBytes`
          val layout = FillerFileLayout.resolveBytes(headBytes)
          require(blockFile.size() == layout.blockSize.longValue,
                  s"the block file length should eq ${layout.blockSize.longValue}")
          Right(RandomFillerFile(layout = layout, underlying = blockFile))
        } catch {
          case t: Throwable ⇒ Left(t)
        }
      }
  }

  def startAppend(ff: Filler.FillerFile): Kleisli[IO, FillerInterpreter.Config, Resp[Long]] = Kleisli { config ⇒
    IO {
      try {
        import RandomFillerFile._
        if (ff.isFull) Left(new IllegalAccessException("the filler file is full"))
        else {
          FillerInterpreter.atomWriting.incrementAndGet()
          Right(ff.tailPos)
        }
      } catch {
        case t: Throwable ⇒ Left(t)
      }
    }
  }

  def endAppend(ff: Filler.FillerFile,
                startPosition: Long,
                endPosition: Long): Kleisli[IO, FillerInterpreter.Config, Resp[FillerFile]] = Kleisli { config ⇒
    IO {
      try {
        import RandomFillerFile._
        FillerInterpreter.atomWriting.decrementAndGet()
        Right(ff.tailPos(endPosition).version(ff.version + 1).versionPos(startPosition).refreshHead())
      } catch {
        case t: Throwable ⇒ Left(t)
      }
    }
  }

  def freeSpace(ff: FillerFile): Kleisli[IO, FillerInterpreter.Config, Resp[Long]] = Kleisli { config ⇒
    IO {
      try {
        import RandomFillerFile._
        Right(ff.freeSpace)
      } catch {
        case t: Throwable ⇒ Left(t)
      }
    }
  }

  def isWriting(ff: FillerFile): Kleisli[IO, FillerInterpreter.Config, Resp[Boolean]] = Kleisli { config ⇒
    IO {
      try {
        Right(FillerInterpreter.atomWriting.get() != 0)
      } catch {
        case t: Throwable ⇒ Left(t)
      }
    }
  }

  def forceToWrite(ff: FillerFile): Kleisli[IO, FillerInterpreter.Config, Resp[Unit]] = Kleisli { config ⇒
    IO {
      try {
        Right(FillerInterpreter.atomWriting.set(0))
      } catch {
        case t: Throwable ⇒ Left(t)
      }

    }
  }
}

object FillerInterpreter {

  val atomWriting = new AtomicInteger(0)

  trait Config {}

  def apply(): FillerInterpreter = new FillerInterpreter() {}
}
