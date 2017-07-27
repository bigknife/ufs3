/**
  * core.scala
  * ----------
  * UFS3 Core program
  * @author bigknife
  * @since 2017/7/14
  */
package ufs3



import cats.data.Kleisli
import cats.Id

import scala.language.higherKinds
import sop._
import kernel.block._
import kernel.fildex._
import kernel.filler._
import kernel.sandwich._

import Fildex._

import ufs3.core.create.CreateProgram
import ufs3.core.fetch.FetchProgroam
import ufs3.core.freespace.FreeSpaceProgram
import ufs3.core.open.OpenProgram
import ufs3.core.read.ReadProgram
import ufs3.core.repair.RepairProgram
import ufs3.core.shutdown.ShutdownProgram
import ufs3.core.write.WriteProgam

import ufs3.kernel.log.Log
import core.data.Data._

package object core {
/*
  ////////////////// READ PROGRAM /////////////////////////////////////////////////////////////////////
  def openForWrite[F[_]](implicit B: Block[F],
                         F: Filler[F],
                         FI: Fildex[F],
                         L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, UFS3]] = OpenProgram.openForWrite[F]

  def openForRead[F[_]](implicit B: Block[F],
                        F: Filler[F],
                        FI: Fildex[F],
                        L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, UFS3]] = OpenProgram.openForRead[F]

  ////////////////// FREE SPACE PROGRAM ////////////////////////////////////////////////////////////////////////////////
  def freeSpaceOfFiller[F[_]](implicit B: Block[F],
                              F: Filler[F],
                              FI: Fildex[F],
                              L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Long]] = FreeSpaceProgram.freeSpaceOfFiller[F]

  def freeSpaceOfFildex[F[_]](implicit B: Block[F],
                              F: Filler[F],
                              FI: Fildex[F],
                              L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Long]] = FreeSpaceProgram.freeSpaceOfFildex[F]

  /////////////////// FETCH PROGRAM ////////////////////////////////////////////////////////////////////////////////////
  def idxOfKey[F[_]](key: String)(implicit B: Block[F],
                                  F: Filler[F],
                                  FI: Fildex[F],
                                  L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Option[Idx]]] =
    FetchProgroam.idxOfKey[F](key)

  def idxOfUuid[F[_]](uuid: String)(implicit B: Block[F],
                                    F: Filler[F],
                                    FI: Fildex[F],
                                    L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Option[Idx]]] =
    FetchProgroam.idxOfUuid[F](uuid)

  def existedKey[F[_]](key: String)(implicit B: Block[F],
                                    F: Filler[F],
                                    FI: Fildex[F],
                                    L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Boolean]] =
    FetchProgroam.existedKey[F](key)

  def list[F[_]](limit: Int, order: String)(implicit B: Block[F],
                                            F: Filler[F],
                                            FI: Fildex[F],
                                            L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Vector[Idx]]] =
    FetchProgroam.list[F]

  ////////////////////  REPAIR PROGRAM ////////////////////////////////////////////////////////////////////////////////
  def repairFildex[F[_]](implicit B: Block[F],
                         F: Filler[F],
                         FI: Fildex[F],
                         L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Unit]] = RepairProgram.repairFildex[F]

  ////////////////////// CREATE PROGRAM ///////////////////////////////////////////////////////////////////////////////
  def create[F[_]](implicit B: Block[F],
                   F: Filler[F],
                   FI: Fildex[F],
                   L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, UFS3]] = CreateProgram.create[F]

  /////////////////// SHUTDOWN PROGRAM /////////////////////////////////////////////////////////////////////////////////
  def shutdown[F[_]](
      ufs3: UFS3)(implicit B: Block[F], F: Filler[F], FI: Fildex[F], L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Unit]] =
    ShutdownProgram.shutdown[F](ufs3)

  ////////////////// WRITE PROGRAM /////////////////////////////////////////////////////////////////////////////////////
  def write[F[_], IN](key: String, in: IN, out: UFS3)(implicit B: Block[F],
                                                      F: Filler[F],
                                                      //BAK: Backup[F],
                                                      L: Log[F],
                                                      FI: Fildex[F],
                                                      S: SandwichIn[F, IN]): Kleisli[Id, CoreConfig, SOP[F, String]] =
    WriteProgam.write[F](key, in, out)

  def readWithKey[F[_], Out](key: String, from: UFS3, to: Out)(implicit B: Block[F],
                                                               F: Filler[F],
                                                               FI: Fildex[F],
                                                               S: SandwichOut[F, Out],
                                                               L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Unit]] =
    ReadProgram.readWithKey[F, Out](key, from, to)
    */
}
