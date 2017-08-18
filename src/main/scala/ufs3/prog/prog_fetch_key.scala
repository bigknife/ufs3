package ufs3.prog

import freestyle._
import ufs3.kernel.commons.{Idx, UFS3}
import ufs3.kernel.modules._

import scala.language.higherKinds

object fetchKey {
  def apply[F[_]](key: String, ufs3: UFS3)(implicit app: App[F]): FreeS[F, Option[Idx]] = {
    for {
      optIdx ← app.store.fildex.fetchKey(key, ufs3.fildexFile.get)
    } yield optIdx
  }
}