package ufs3.prog

import freestyle._
import ufs3.kernel.commons._
import ufs3.kernel.modules._

import scala.language.higherKinds

object open {
  def apply[F[_]](mode: FileMode)(implicit app: App[F]): FreeS[F, UFS3] = {
    for {
      fillerFilePath  ← app.readerM.reader(_.fillerBlockPath)
      fillerBlockFile ← app.store.block.open(fillerFilePath, mode)
      fildexBlockFile ← app.store.block.open(fillerFilePath.indexPath.value, mode)
      fillerFile      ← app.store.filler.check(fillerBlockFile)
      idxOk           ← app.store.fildex.check(fildexBlockFile, fillerFile)
      _               ← app.errorM.either(Either.cond(!idxOk, (), new IllegalStateException("the idx need to be repaired")))
      fildexFile      ← app.store.fildex.load(fildexBlockFile)
      locked          ← if (mode == FileMode.ReadWrite) app.store.block.lock(fillerBlockFile): FreeS[F, Boolean] else FreeS.pure[F, Boolean](true)
      _               ← app.errorM.either(Either.cond(locked, (), new IllegalStateException("filler file can't be locked")))
    } yield UFS3(fillerBlockFile, fillerFile, fildexFile)
  }
}
