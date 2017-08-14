package ufs3.prog
import cats.free.FreeApplicative.FA
import cats.free.{Free, FreeApplicative}
import ufs3.kernel.modules._

import scala.language.higherKinds
import freestyle._
import ufs3.kernel.commons
import ufs3.kernel.commons.Config

object create {
  import cats.implicits._
  import freestyle.implicits._

  def apply[F[_]](implicit app: App[F]): FreeS[F, Unit] = {
    val s = app.readerM.reader[Config](x ⇒ x).freeS
    for {
      fillerPath ← app.readerM.reader(config ⇒ config.fillerBlockPath)
      fillerSize ← app.readerM.reader(config ⇒ config.fillerBlockSize)
      idxSize    ← app.readerM.reader(config ⇒ config.idxBlockSize)
      bfFiller   ← app.store.block.create(fillerPath, fillerSize)
      bfFildex   ← app.store.block.create(fillerPath.indexPath.value, idxSize)
      _          ← app.store.filler.init(bfFiller)
      _          ← app.store.fildex.init(bfFildex)
      _          ← app.store.block.close(bfFiller)
      _          ← app.store.block.close(bfFildex)
    } yield ()
  }

}
