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
import ufs3.interpreter.layout.FillerFileLayout


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
      // TODO re-think check logic
      // 0. check the size if >= head size
      // 1. read all head
      // 2. check if magic eq the 'FILL'
      // 3. check if blockSize eq blockFile.size

      import RandomAccessBlockFile._
      require(blockFile.size() >= RandomFillerFile.HEAD_SIZE,
        "the block file is not a FillerFile, file size is less than HEAD_SIZE")

      val headBytes = {
        blockFile.seek(0); blockFile.read(RandomFillerFile.HEAD_SIZE)
      }.array()
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

  def endAppend(ff: Filler.FillerFile, endPosition: Long): Kleisli[IO, FillerInterpreter.Config, Unit] = Kleisli {config ⇒
    IO {
      import RandomFillerFile._
      ff.tailPos(endPosition).refreshHead()
    }
  }
}

object FillerInterpreter {
  trait Config {}

  def apply(): FillerInterpreter = new FillerInterpreter(){}
}
