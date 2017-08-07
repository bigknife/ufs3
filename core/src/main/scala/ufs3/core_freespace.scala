/**
  * core_freespace.scala
  * ----------
  * free space of idx and filler program
  * @author bigknife
  * @since 2017/7/27
  */
package ufs3
package core
package freespace

import cats.Id
import cats.data.Kleisli
import sop._
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import core.data.Data._
import scala.language.higherKinds
import ufs3.core.open.OpenProgram._
import RespSOP._
trait FreeSpaceProgram {
  /**query free space of filler index file*/
  def freeSpaceOfFildex[F[_]](implicit B: Block[F],
                              F: Filler[F],
                              FI: Fildex[F],
                              L: Log[F]): Kleisli[Id, CoreConfig, RespSOP[F, Long]] = {
    openForRead[F].andThen[RespSOP[F, Long]]((x: RespSOP[F, UFS3]) ⇒ {
      for {
        ufs3 ← x.asM
        l    ← FI.freeSpace(ufs3.fildexFile.get()).asM
      } yield l
    })
  }

  /**query free space of filler file*/
  def freeSpaceOfFiller[F[_]](implicit B: Block[F],
                              F: Filler[F],
                              FI: Fildex[F],
                              L: Log[F]): Kleisli[Id, CoreConfig, RespSOP[F, Long]] = {
    openForRead[F].andThen[RespSOP[F, Long]]((x: RespSOP[F, UFS3]) ⇒ {
      for {
        ufs3 ← x.asM
        l    ← F.freeSpace(ufs3.fillerFile.get()).asM
      } yield l
    })
  }
}

object FreeSpaceProgram extends FreeSpaceProgram
