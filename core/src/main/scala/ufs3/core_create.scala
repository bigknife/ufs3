/**
  * core_create.scala
  * ----------
  * create ufs3(filler and fildex) program
  * @author bigknife
  * @since 2017/7/27
  */
package ufs3
package core
package create

import cats.Id
import cats.data.Kleisli
import sop._
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log

import scala.language.higherKinds
import core.data.Data._

trait CreateProgram {

  def create[F[_]](implicit B: Block[F],
                   F: Filler[F],
                   FI: Fildex[F],
                   L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Unit]] =
    Kleisli { coreConfig ⇒
      val path = coreConfig.fillerBlockPath
      val size    = coreConfig.fillerBlockSize
      val idxSize = coreConfig.idxBlockSize

      val pathString    = path.file.value.getAbsolutePath
      val idxPath       = path.indexPath
      val idxPathString = idxPath.file.value.getAbsolutePath

      import L._

      val prog: Id[SOP[F, Unit]] = for {
        _           ← debug(s"try to create UFS3 system at path: $pathString and index path: $idxPathString")
        _           ← debug(s"check filler existing $pathString")
        fillerBeing ← B.existed(path)
        _           ← debug(s"check fildex existing $idxPathString")
        fildexBeing ← B.existed(idxPath)
        _ ← if (!fillerBeing && !fildexBeing) {
          for {
            bfFiller ← B.create(path, size)
            _        ← F.init(bfFiller)
            _        ← info(s"created filler: $path")
            bfFildex ← B.create(idxPath, idxSize)
            _        ← FI.init(bfFildex)
            _        ← info(s"created fildex: $idxPathString")
            _        ← B.close(bfFildex)
            _        ← B.close(bfFiller)
          } yield ()
        } else {
          warn("filler or fildex has existed")
        } : SOP[F, Unit]
      } yield ()
      prog
    }
}

object CreateProgram extends CreateProgram
