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

trait FetchProgroam {
  def idxOfKey[F[_]](key: String)(implicit B: Block[F],
                                  F: Filler[F],
                                  FI: Fildex[F],
                                  L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Option[Idx]]] = {

    openForRead[F].andThen[SOP[F, Option[Idx]]]((x: SOP[F, UFS3]) ⇒ {
      for {
        ufs3 ← x
        idx  ← FI.fetchKey(key, ufs3.fildexFile.get())
      } yield idx
    })
  }

  def idxOfUuid[F[_]](uuid: String)(implicit B: Block[F],
                                    F: Filler[F],
                                    FI: Fildex[F],
                                    L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Option[Idx]]] = {

    openForRead[F].andThen[SOP[F, Option[Idx]]]((x: SOP[F, UFS3]) ⇒ {
      for {
        ufs3 ← x
        idx  ← FI.fetchUuid(uuid, ufs3.fildexFile.get())
      } yield idx
    })
  }

  def existedKey[F[_]](key: String)(implicit B: Block[F],
                                    F: Filler[F],
                                    FI: Fildex[F],
                                    L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Boolean]] = {
    openForRead[F].andThen[SOP[F, Boolean]]((x: SOP[F, UFS3]) ⇒ {
      for {
        ufs3 ← x
        e    ← FI.fetchKey(key, ufs3.fildexFile.get())
        _    ← F.close(ufs3.fillerFile.get())
        _    ← FI.close(ufs3.fildexFile.get())
      } yield e.nonEmpty
    })
  }

  def list[F[_]](limit: Int, order: String)(implicit B: Block[F],
                                            F: Filler[F],
                                            FI: Fildex[F],
                                            L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Vector[Idx]]] = {
    openForRead[F].andThen[SOP[F, Vector[Idx]]]((x: SOP[F, UFS3]) ⇒
      for {
        ufs3 ← x
        idx  ← FI.query(limit, Fildex.Order(order), ufs3.fildexFile.get())
      } yield idx)
  }
}
object FetchProgroam extends FetchProgroam
