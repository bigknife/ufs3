package ufs3.prog

import freestyle._
import ufs3.kernel.modules._
import ufs3.kernel.commons._

import scala.language.higherKinds

object close {

  def apply[F[_]](ufs3: UFS3)(implicit app: App[F]): FreeS[F, Unit] = {
    for {
      _ ← app.store.filler.close(ufs3.fillerFile.get)
      _ ← app.store.fildex.close(ufs3.fildexFile.get)
      _ ← app.store.block.unlock(ufs3.blockFile.get)
      _ ← app.store.block.close(ufs3.blockFile.get)
    } yield ()
  }
}