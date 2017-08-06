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
import RespSOP._

trait CreateProgram {

  def create[F[_]](implicit B: Block[F],
                   F: Filler[F],
                   FI: Fildex[F],
                   L: Log[F]): Kleisli[Id, CoreConfig, RespSOP[F, Unit]] =
    Kleisli { coreConfig ⇒
      val path = coreConfig.fillerBlockPath
      val size    = coreConfig.fillerBlockSize
      val idxSize = coreConfig.idxBlockSize

      val pathString    = path.file.value.getAbsolutePath
      val idxPath       = path.indexPath
      val idxPathString = idxPath.file.value.getAbsolutePath

      import L._

      val createProg: RespSOP[F, Unit] = for {
        bfFiller ← B.create(path, size).asM
        _ ← F.init(bfFiller).asM
        _ ← info(s"created filler: $path").asM
        bfFildex ← B.create(idxPath, idxSize).asM
        _ ← FI.init(bfFildex).asM
        _ ← info(s"created fildex: $idxPathString").asM
        _ ← B.close(bfFildex).asM
        _ ← B.close(bfFiller).asM
      } yield ()

      val warnProg: RespSOP[F, Unit] = for {
        _ ← warn("filler or fildex has existed").asM
      } yield ()


      val prog: Id[RespSOP[F, Unit]] = for {
        _           ← debug(s"check filler and fildex existing $pathString").asM
        fillerBeing ← B.existed(path).asM
        fildexBeing ← B.existed(idxPath).asM
        _ ← (if (fillerBeing || fildexBeing) warnProg else createProg).asM
      } yield ()
      prog
    }
}

object CreateProgram extends CreateProgram
