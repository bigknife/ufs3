/**
  * core_fetch.scala
  * ----------
  * fetch program
  * @author bigknife
  * @since 2017/7/27
  */
package ufs3
package core
package fetch

import cats.Id
import cats.data.Kleisli
import sop._
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.fildex.Fildex.Idx
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import core.data.Data._
import ufs3.core.open.OpenProgram._
import scala.language.higherKinds
import RespSOP._

trait FetchProgroam {
  def idxOfKey[F[_]](key: String)(implicit B: Block[F],
                                  F: Filler[F],
                                  FI: Fildex[F],
                                  L: Log[F]): Kleisli[Id, CoreConfig, RespSOP[F, Option[Idx]]] = {

    openForRead[F].andThen[RespSOP[F, Option[Idx]]]((x: RespSOP[F, UFS3]) ⇒ {
      for {
        ufs3 ← x.asM
        idx  ← FI.fetchKey(key, ufs3.fildexFile.get()).asM
      } yield idx
    })
  }

  def idxOfUuid[F[_]](uuid: String)(implicit B: Block[F],
                                    F: Filler[F],
                                    FI: Fildex[F],
                                    L: Log[F]): Kleisli[Id, CoreConfig, RespSOP[F, Option[Idx]]] = {

    openForRead[F].andThen[RespSOP[F, Option[Idx]]]((x: RespSOP[F, UFS3]) ⇒ {
      for {
        ufs3 ← x.asM
        idx  ← FI.fetchUuid(uuid, ufs3.fildexFile.get()).asM
      } yield idx
    })
  }

  def existedKey[F[_]](key: String)(implicit B: Block[F],
                                    F: Filler[F],
                                    FI: Fildex[F],
                                    L: Log[F]): Kleisli[Id, CoreConfig, RespSOP[F, Boolean]] = {
    openForRead[F].andThen[RespSOP[F, Boolean]]((x: RespSOP[F, UFS3]) ⇒ {
      for {
        ufs3 ← x.asM
        e    ← FI.fetchKey(key, ufs3.fildexFile.get()).asM
        _    ← F.close(ufs3.fillerFile.get()).asM
        _    ← FI.close(ufs3.fildexFile.get()).asM
      } yield e.nonEmpty
    })
  }

  def list[F[_]](limit: Int, order: String)(implicit B: Block[F],
                                            F: Filler[F],
                                            FI: Fildex[F],
                                            L: Log[F]): Kleisli[Id, CoreConfig, RespSOP[F, Vector[Idx]]] = {
    openForRead[F].andThen[RespSOP[F, Vector[Idx]]]((x: RespSOP[F, UFS3]) ⇒
      for {
        ufs3 ← x.asM
        idx  ← FI.query(limit, Fildex.Order(order), ufs3.fildexFile.get()).asM
      } yield idx)
  }
}
object FetchProgroam extends FetchProgroam
