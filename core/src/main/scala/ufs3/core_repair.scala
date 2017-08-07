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
import RespSOP._
import ufs3.kernel.fildex.Fildex.FildexFile

trait RepairProgram {
  def repairFildex[F[_]](implicit B: Block[F],
                         F: Filler[F],
                         FI: Fildex[F],
                         L: Log[F]): Kleisli[Id, CoreConfig, RespSOP[F, Unit]] = {
    Kleisli { config ⇒
      val path       = config.fillerBlockPath
      import L._

      val createIdxProg: RespSOP[F, Unit] = for {
        _ ← warn("index file lost, create a new index file now").asM
        a ← B.create(path.indexPath, config.idxBlockSize).asM
        _ ← FI.init(a).asM
        _ ← info(s"a new index file created and initialized: ${path.indexPath.file.value}").asM
      } yield ()

      val prog: Id[RespSOP[F, Unit]] = for {
        fildexBeing ← B.existed(path.indexPath).asM
        _ ← (if (fildexBeing) RespSOP.pure[F, Unit](()) else createIdxProg).asM
        bf    ← B.open(path, FileMode.ReadWrite).asM
        bfi   ← B.open(path.indexPath, FileMode.ReadWrite).asM
        ff    ← F.check(bf).asM
        idxOk ← FI.check(bfi, ff).asM
        _ ← if (idxOk) RespSOP.pure[F, Unit](()).asM else {
          val s: RespSOP[F, Unit] = for {
            _ ← FI.repair(bfi, ff).asM
          } yield ()
          s.asM
        }
      } yield ()
      prog
    }
  }
}

object RepairProgram extends RepairProgram