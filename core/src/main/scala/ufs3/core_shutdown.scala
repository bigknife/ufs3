/**
  * core_shutdown.scala
  * ----------
  * shutdown program
  * @author bigknife
  * @since 2017/7/27
  */
package ufs3
package core
package shutdown

import cats.Id
import cats.data.Kleisli
import sop._
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import core.data.Data._
import scala.language.higherKinds
import RespSOP._
trait ShutdownProgram {
  def shutdown[F[_]](
      ufs3: UFS3)(implicit B: Block[F], F: Filler[F], FI: Fildex[F], L: Log[F]): Kleisli[Id, CoreConfig, RespSOP[F, Unit]] =
    Kleisli { coreConfig ⇒
      import L._
      val prog: Id[RespSOP[F, Unit]] = for {
        _ ← debug("close filler file").asM
        _ ← F.close(ufs3.fillerFile.get()).asM
        _ ← debug("close fildex file").asM
        _ ← FI.close(ufs3.fildexFile.get()).asM
        _ ← debug("unlock block file").asM
        _ ← B.unlock(ufs3.blockFile.get()).asM
        _ ← debug("close block file").asM
        _ ← B.close(ufs3.blockFile.get()).asM
      } yield ()
      prog
    }
}
object ShutdownProgram extends ShutdownProgram
