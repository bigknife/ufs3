package ufs3.prog

import freestyle._
import ufs3.kernel.commons.UFS3
import ufs3.kernel.modules._

import scala.language.higherKinds

object existedKey {
  def apply[F[_]](key: String, ufs3: UFS3)(implicit app: App[F]): FreeS[F, Boolean] = {
    for {
      optIdx ← app.store.fildex.fetchKey(key, ufs3.fildexFile.get)
    } yield optIdx.nonEmpty
  }
}