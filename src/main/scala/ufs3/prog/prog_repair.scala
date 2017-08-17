package ufs3.prog

import java.io.OutputStream
import java.nio.ByteBuffer

import ufs3.kernel.commons.{FileMode, UFS3}
import ufs3.kernel.modules.App
import freestyle._
import ufs3.kernel.exceptions._

import scala.language.higherKinds

object repair {
  def apply[F[_]](implicit app: App[F]): FreeS[F, Unit] = {
    val createIdxProg: FreeS[F, Unit] = for {
      _            ← app.log.warn("index file lost, create a new index file now", None)
      fillerPath   ← app.readerM.reader(_.fillerBlockPath)
      idxBlockSize ← app.readerM.reader(_.idxBlockSize)
      a            ← app.store.block.create(fillerPath.indexPath.value, idxBlockSize)
      _            ← app.store.fildex.init(a)
      _            ← app.log.info(s"a new index file created and initialized: ${fillerPath.indexPath.value.file.value}")
    } yield ()

    for {
      fillerPath  ← app.readerM.reader(_.fillerBlockPath)
      fildexBeing ← app.store.block.existed(fillerPath.indexPath.value)
      _           ← if (!fildexBeing) createIdxProg else FreeS.pure[F, Unit](())
      bf          ← app.store.block.open(fillerPath, FileMode.ReadWrite)
      bfi         ← app.store.block.open(fillerPath.indexPath.value, FileMode.ReadWrite)
      ff          ← app.store.filler.check(bf)
      idxOk       ← app.store.fildex.check(bfi, ff)
      _ ← if (!idxOk) {
        val s: FreeS[F, Unit] = for { _ ← app.store.fildex.repair(bfi, ff) } yield ()
        s
      } else app.log.info("the idx file is healthy.", None): FreeS[F, Unit]
      _ ← app.store.block.close(bf)
      _ ← app.store.block.close(bfi)
    } yield ()
  }
}
