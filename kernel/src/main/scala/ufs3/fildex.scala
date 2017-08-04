/**
  * fildex.scala
  * ----------
  * Filler File Index
  * @author bigknife
  * @since 2017/7/13
  */
package ufs3
package kernel
package fildex

import java.io.RandomAccessFile
import java.nio.ByteBuffer
import java.nio.channels.FileChannel

import cats.Eval
import cats.free.Inject
import sop._
import ufs3.kernel.block._
import Block._
import ufs3.kernel.filler.Filler.FillerFile

import scala.language.{higherKinds, implicitConversions}

trait Fildex[F[_]] {
  import Fildex._

  def init(bf: BlockFile): RespPar[F, FildexFile]
  def check(bf: BlockFile, filler: FillerFile): RespPar[F, Boolean]
  def repair(bf: BlockFile, filler: FillerFile): RespPar[F, FildexFile]
  def load(bf: BlockFile): RespPar[F, FildexFile]
  def append(bf: FildexFile, idx: Idx): RespPar[F, FildexFile]
  def close(bf: FildexFile): RespPar[F, Unit]
  def fetchKey(key: String, fildex: FildexFile): RespPar[F, Option[Idx]]
  def fetchUuid(uuid: String, fildex: FildexFile): RespPar[F, Option[Idx]]
  def query(limit: Int, order: Order, fildex: FildexFile): RespPar[F, Vector[Idx]]
  def freeSpace(fi: FildexFile): RespPar[F, Long]
}

object Fildex {
  sealed trait Order
  case object Asc  extends Order
  case object Desc extends Order
  object Order {
    def apply(o: String): Order = o match {
      case "desc" ⇒ Desc
      case _      ⇒ Asc
    }
  }

  sealed trait Op[A]
  final case class Init(bf: BlockFile)                                 extends Op[Resp[FildexFile]]
  final case class Check(bf: BlockFile, filler: FillerFile)            extends Op[Resp[Boolean]]
  final case class Repair(bf: BlockFile, filler: FillerFile)           extends Op[Resp[FildexFile]]
  final case class Load(bf: BlockFile)                                 extends Op[Resp[FildexFile]]
  final case class Close(bf: FildexFile)                               extends Op[Resp[Unit]]
  final case class FetchKey(key: String, fildex: FildexFile)           extends Op[Resp[Option[Idx]]]
  final case class FetchUuid(uuid: String, fildex: FildexFile)         extends Op[Resp[Option[Idx]]]
  final case class Query(limit: Int, order: Order, fildex: FildexFile) extends Op[Resp[Vector[Idx]]]
  final case class FreeSpace(fi: FildexFile)                           extends Op[Resp[Long]]
  final case class Append(bf: FildexFile, idx: Idx)                    extends Op[Resp[FildexFile]]

  class To[F[_]](implicit I: Inject[Op, F]) extends Fildex[F] {
    def check(bf: BlockFile, filler: FillerFile): RespPar[F, Boolean] = liftPar_T[Op, F, Resp[Boolean]](Check(bf, filler))
    def repair(bf: BlockFile, filler: FillerFile): RespPar[F, FildexFile] =
      liftPar_T[Op, F, Resp[FildexFile]](Repair(bf, filler))
    def load(bf: BlockFile): RespPar[F, FildexFile] = liftPar_T[Op, F, Resp[FildexFile]](Load(bf))
    def init(bf: BlockFile): RespPar[F, FildexFile] = liftPar_T[Op, F, Resp[FildexFile]](Init(bf))
    def close(bf: FildexFile): RespPar[F, Unit]     = liftPar_T[Op, F, Resp[Unit]](Close(bf))
    def fetchKey(key: String, fildex: FildexFile): RespPar[F, Option[Idx]] =
      liftPar_T[Op, F, Resp[Option[Idx]]](FetchKey(key, fildex))
    def fetchUuid(uuid: String, fildex: FildexFile): RespPar[F, Option[Idx]] =
      liftPar_T[Op, F, Resp[Option[Idx]]](FetchUuid(uuid, fildex))
    def query(limit: Int, order: Order, fildex: FildexFile): RespPar[F, Vector[Idx]] =
      liftPar_T[Op, F, Resp[Vector[Idx]]](Query(limit, order, fildex))
    def append(bf: FildexFile, idx: Idx): RespPar[F, FildexFile] = liftPar_T[Op, F, Resp[FildexFile]](Append(bf, idx))
    def freeSpace(fi: FildexFile): RespPar[F, Long]              = liftPar_T[Op, F, Resp[Long]](FreeSpace(fi))
  }
  implicit def to[F[_]](implicit I: Inject[Op, F]): Fildex[F] = new To[F]

  def apply[F[_]](implicit F: Fildex[F]) = F

  trait Handler[M[_]] extends NT[Op, M] {

    def check(bf: BlockFile, filler: FillerFile): M[Resp[Boolean]]
    def repair(bf: BlockFile, filler: FillerFile): M[Resp[FildexFile]]
    def load(bf: BlockFile): M[Resp[FildexFile]]
    def init(bf: BlockFile): M[Resp[FildexFile]]
    def append(bf: FildexFile, idx: Idx): M[Resp[FildexFile]]
    def close(bf: FildexFile): M[Resp[Unit]]
    def fetchKey(key: String, fildex: FildexFile): M[Resp[Option[Idx]]]
    def fetchUuid(uuid: String, fildex: FildexFile): M[Resp[Option[Idx]]]
    def query(limit: Int, order: Order, fildex: FildexFile): M[Resp[Vector[Idx]]]
    def freeSpace(fi: FildexFile): M[Resp[Long]]

    def apply[A](fa: Op[A]): M[A] = fa match {
      case Check(bf, filler)           ⇒ check(bf, filler)
      case Load(bf)                    ⇒ load(bf)
      case Repair(bf, filler)          ⇒ repair(bf, filler)
      case Init(bf)                    ⇒ init(bf)
      case Close(bf)                   ⇒ close(bf)
      case Append(bf, idx)             ⇒ append(bf, idx)
      case FetchKey(key, fildex)       ⇒ fetchKey(key, fildex)
      case FetchUuid(uuid, fildex)     ⇒ fetchUuid(uuid, fildex)
      case Query(limit, order, fildex) ⇒ query(limit, order, fildex)
      case FreeSpace(fi)               ⇒ freeSpace(fi)
    }
  }

  trait FildexFile

  trait Data

  // Index structure
  final case class Idx(key: String, uuid: String, startPoint: Long, endPoint: Long) {
    require(key.getBytes("utf-8").length == 32, "fildex index key should be 32bit String")
    require(uuid.getBytes("utf-8").length == 32, "fildex index uuid should be 32bit String")

    def byteBuffer: ByteBuffer = {
      val bb = ByteBuffer.allocate(80)
      bb.put(key.getBytes("utf-8"))
      bb.put(uuid.getBytes("utf-8"))
      bb.putLong(startPoint)
      bb.putLong(endPoint)
      bb.flip()
      bb
    }

    def fileLength: Long = endPoint - startPoint
  }

}
