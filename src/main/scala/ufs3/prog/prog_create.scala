package ufs3.prog

import freestyle._
import ufs3.kernel.modules._

import scala.language.higherKinds

object create {

  def apply[F[_]](implicit app: App[F]): FreeS[F, Unit] = {
    for {
      fillerPath ← app.readerM.reader(_.fillerBlockPath)
      _          ← app.log.debug(s"fillerPath: ${fillerPath.file.value}")
      _          ← app.log.debug(s"fildexPath: ${fillerPath.indexPath.value.file.value}")
      fillerSize ← app.readerM.reader(_.fillerBlockSize)
      _          ← app.log.debug(s"fillerSize: ${fillerSize.toStringWithUnit("M")}")
      idxSize    ← app.readerM.reader(_.idxBlockSize)
      _          ← app.log.debug(s"idxSize: ${idxSize.toStringWithUnit("M")}")
      existed    ← app.store.block.existed(fillerPath)
      b ← app.errorM.either(
        Either.cond[Throwable, Unit](!existed, (), new IllegalAccessException(s"${fillerPath.file.value} existed")))
      bfFiller ← app.store.block.create(fillerPath, fillerSize)
      _        ← app.log.info("filler created")
      bfFildex ← app.store.block.create(fillerPath.indexPath.value, idxSize)
      _        ← app.log.info("fildex created")
      _        ← app.store.filler.init(bfFiller)
      _        ← app.log.info("filler initialized")
      _        ← app.store.fildex.init(bfFildex)
      _        ← app.log.info("fildex initialized")
      _        ← app.store.block.close(bfFiller)
      _        ← app.store.block.close(bfFildex)
    } yield ()
  }
}
