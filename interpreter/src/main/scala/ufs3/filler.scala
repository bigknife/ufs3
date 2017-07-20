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

import cats.data.Kleisli
import ufs3.kernel.filler._
import cats.effect.IO
import ufs3.kernel.block.Block
import ufs3.interpreter.block.RandomAccessBlockFile
import ufs3.interpreter.layout.FillerFileLayout
import ufs3.kernel.filler.Filler.FillerFile


/**
  * Filler Interpreter
  * Filler File Layout: @see {FillerFileLayout}
  */
trait FillerInterpreter extends Filler.Handler[Kleisli[IO, FillerInterpreter.Config, ?]] {

  def init(blockFile: Block.BlockFile): Kleisli[IO, FillerInterpreter.Config, Filler.FillerFile] = Kleisli { config ⇒
    // init: seek to 0, and insert filler head bytes
    IO {
      import RandomAccessBlockFile._
      val layout = FillerFileLayout(blockFile.size())
      RandomFillerFile(layout, underlying = blockFile).init()
    }
  }

  def close(ff: Filler.FillerFile): Kleisli[IO, FillerInterpreter.Config, Unit] = Kleisli { config ⇒
    IO {
      // now nothing to do here
    }
  }

  def check(blockFile: Block.BlockFile): Kleisli[IO, FillerInterpreter.Config, Filler.FillerFile] = Kleisli { config ⇒
    IO {
      // 0. check the size if >= head size
      // 1. read all head
      // 2. check if magic eq the 'FILL'
      // 3. check if blockSize eq blockFile.size

      import RandomAccessBlockFile._
      require(blockFile.size() >= FillerFileLayout.HEAD_SIZE,
        s"the block file lenght should be greater than ${FillerFileLayout.HEAD_SIZE}")

      val headBytes = {
        blockFile.seek(0)
        val bb = blockFile.read(FillerFileLayout.HEAD_SIZE)
        val bytes = new Array[Byte](FillerFileLayout.HEAD_SIZE.toInt)
        bb.get(bytes)
        bytes
      }
      // the magic check is in the `resoveBytes`
      val layout = FillerFileLayout.resolveBytes(headBytes)
      RandomFillerFile(layout = layout, underlying = blockFile)

    }
  }

  def startAppend(ff: Filler.FillerFile): Kleisli[IO, FillerInterpreter.Config, Long] = Kleisli { config ⇒
    IO {
      import RandomFillerFile._
      ff.tailPos
    }
  }

  def endAppend(ff: Filler.FillerFile, startPosition: Long, endPosition: Long): Kleisli[IO, FillerInterpreter.Config, FillerFile] = Kleisli {config ⇒
    IO {
      import RandomFillerFile._
      ff.tailPos(endPosition).version(ff.version + 1).versionPos(startPosition).refreshHead()
    }
  }
}

object FillerInterpreter {
  trait Config {}

  def apply(): FillerInterpreter = new FillerInterpreter(){}
}
