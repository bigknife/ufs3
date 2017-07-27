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

trait OpenProgram {
  private def _open[F[_]](mode: Block.FileMode)(implicit B: Block[F],
                                               F: Filler[F],
                                               FI: Fildex[F],
                                               L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, UFS3]] = {
    Kleisli { config ⇒
      val path       = config.fillerBlockPath
      val pathString = path.file.value.getAbsolutePath
      val prog: Id[SOP[F, UFS3]] = for {
        being ← B.existed(path)
        _     ← if (!being) throw new IOException(s"file not exists: $pathString") else SOP.pure[F, Unit](())
        bf    ← B.open(path, mode)
        bfi   ← B.open(path.indexPath, mode)
        ff    ← F.check(bf)
        idxOk ← FI.check(bfi, ff)
        fif   ← if (idxOk) FI.load(bfi) else throw new IllegalStateException("fildex is not legal, please repair it")
      } yield UFS3(bf, ff, fif)
      prog
    }
  }

  def openForWrite[F[_]](implicit B: Block[F],
                         F: Filler[F],
                         FI: Fildex[F],
                         L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, UFS3]] = _open(FileMode.ReadWrite)

  def openForRead[F[_]](implicit B: Block[F],
                        F: Filler[F],
                        FI: Fildex[F],
                        L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, UFS3]] = _open(FileMode.ReadOnly)
}

object OpenProgram extends OpenProgram