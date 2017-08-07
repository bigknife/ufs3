/**
  * core_open.scala
  * ----------
  * open program
  * @author bigknife
  * @since 2017/7/27
  */
package ufs3
package core
package open

import java.io.IOException

import cats.Id
import cats.data.Kleisli
import sop._
import ufs3.kernel.block.Block
import ufs3.kernel.block.Block.FileMode
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import core.data.Data._

import scala.language.higherKinds
import RespSOP._
import ufs3.kernel.fildex.Fildex.FildexFile

trait OpenProgram {
  private def _open[F[_]](mode: Block.FileMode)(implicit B: Block[F],
                                               F: Filler[F],
                                               FI: Fildex[F],
                                               L: Log[F]): Kleisli[Id, CoreConfig, RespSOP[F, UFS3]] = {
    Kleisli { config ⇒
      val path       = config.fillerBlockPath
      val prog: Id[RespSOP[F, UFS3]] = for {
        bf    ← B.open(path, mode).asM
        bfi   ← B.open(path.indexPath, mode).asM
        ff    ← F.check(bf).asM
        idxOk ← FI.check(bfi, ff).asM
        fif   ← if (idxOk) FI.load(bfi).asM else RespSOP.error[F, FildexFile](new IllegalStateException("the idx need to be repaired")).asM
      } yield UFS3(bf, ff, fif)
      prog
    }
  }

  def openForWrite[F[_]](implicit B: Block[F],
                         F: Filler[F],
                         FI: Fildex[F],
                         L: Log[F]): Kleisli[Id, CoreConfig, RespSOP[F, UFS3]] = _open(FileMode.ReadWrite)

  def openForRead[F[_]](implicit B: Block[F],
                        F: Filler[F],
                        FI: Fildex[F],
                        L: Log[F]): Kleisli[Id, CoreConfig, RespSOP[F, UFS3]] = _open(FileMode.ReadOnly)
}

object OpenProgram extends OpenProgram