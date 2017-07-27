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

trait ShutdownProgram {
  def shutdown[F[_]](
      ufs3: UFS3)(implicit B: Block[F], F: Filler[F], FI: Fildex[F], L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Unit]] =
    Kleisli { coreConfig ⇒
      import L._
      val prog: Id[SOP[F, Unit]] = for {
        _ ← debug("close filler file")
        _ ← F.close(ufs3.fillerFile.get())
        _ ← debug("close fildex file")
        _ ← FI.close(ufs3.fildexFile.get())
        _ ← debug("unlock block file")
        _ ← B.unlock(ufs3.blockFile.get())
        _ ← debug("close block file")
        _ ← B.close(ufs3.blockFile.get())
      } yield ()
      prog
    }
}
object ShutdownProgram extends ShutdownProgram
