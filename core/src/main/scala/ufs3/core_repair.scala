/**
  * core_repair.scala
  * ----------
  * repair filler index file program
  * @author bigknife
  * @since 2017/7/27
  */

package ufs3
package core
package repair

import java.io.IOException

import sop._
import cats.Id
import cats.data.Kleisli
import ufs3.kernel.block.Block
import ufs3.kernel.block.Block.FileMode
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import core.data.Data._
import scala.language.higherKinds

trait RepairProgram {
  def repairFildex[F[_]](implicit B: Block[F],
                         F: Filler[F],
                         FI: Fildex[F],
                         L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Unit]] = {
    Kleisli { config ⇒
      val path       = config.fillerBlockPath
      val pathString = path.file.value.getAbsolutePath

      import L._
      val prog: Id[SOP[F, Unit]] = for {
        being       ← B.existed(path)
        _           ← if (!being) throw new IOException(s"file not exists: $pathString") else SOP.pure[F, Unit](())
        fildexBeing ← B.existed(path.indexPath)
        _ ← (if (fildexBeing) info(s"index file existed: ${path.indexPath.file.value}")
        else
          for {
            _ ← warn("index file lost, create a new index file now")
            a ← B.create(path.indexPath, config.idxBlockSize)
            _ ← FI.init(a)
            _ ← info(s"a new index file created and initialized: ${path.indexPath.file.value}")
          } yield ()): SOP[F, Unit]

        bf    ← B.open(path, FileMode.ReadWrite)
        bfi   ← B.open(path.indexPath, FileMode.ReadWrite)
        ff    ← F.check(bf)
        idxOk ← FI.check(bfi, ff)
        _ ← (if (idxOk) info("the index file is consistent with filler file, unnecessary to repair.")
        else {
          for {
            _ ← info("start to repair the index file")
            _ ← FI.repair(bfi, ff)
            _ ← info("repair successfully!")
          } yield ()
        }): SOP[F, Unit]
      } yield ()
      prog
    }
  }
}

object RepairProgram extends RepairProgram