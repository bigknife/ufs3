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
import ufs3.kernel.filler.Filler
import ufs3.interpreter.block._
import ufs3.interpreter.filler._
import ufs3.kernel.block.Block

trait FildexInterpreter extends Fildex.Handler[Kleisli[IO, FildexInterpreter.Config, ?]] {
  import RandomAccessBlockFile._
  import RandomFillerFile._

  def init(ff: Block.BlockFile): Kleisli[IO, FildexInterpreter.Config, Fildex.FildexFile] = Kleisli {config ⇒
    ???
  }
}

object FildexInterpreter {
  trait Config {
    // index file block size
    def fildexSize: Long
  }
}