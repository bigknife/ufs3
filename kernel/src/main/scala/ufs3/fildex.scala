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

  def init(bf: BlockFile): Par[F, FildexFile]
  def check(bf: BlockFile, filler: FillerFile): Par[F, Boolean]
  def repair(bf: BlockFile, filler: FillerFile): Par[F, FildexFile]
  def load(bf: BlockFile): Par[F, FildexFile]
  def append(bf: FildexFile, idx: Idx): Par[F, FildexFile]
  def close(bf: FildexFile): Par[F, Unit]
  def fetch(key: String, fildex: FildexFile): Par[F, Option[Idx]]
  def query(limit: Int, order: Order, fildex: FildexFile): Par[F, Vector[Idx]]
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
  final case class Init(bf: BlockFile)                                 extends Op[FildexFile]
  final case class Check(bf: BlockFile, filler: FillerFile)            extends Op[Boolean]
  final case class Repair(bf: BlockFile, filler: FillerFile)           extends Op[FildexFile]
  final case class Load(bf: BlockFile)                                 extends Op[FildexFile]
  final case class Close(bf: FildexFile)                               extends Op[Unit]
  final case class Fetch(key: String, fildex: FildexFile)              extends Op[Option[Idx]]
  final case class Query(limit: Int, order: Order, fildex: FildexFile) extends Op[Vector[Idx]]

  final case class Append(bf: FildexFile, idx: Idx) extends Op[FildexFile]

  class To[F[_]](implicit I: Inject[Op, F]) extends Fildex[F] {
    def check(bf: BlockFile, filler: FillerFile): Par[F, Boolean]     = liftPar_T[Op, F, Boolean](Check(bf, filler))
    def repair(bf: BlockFile, filler: FillerFile): Par[F, FildexFile] = liftPar_T[Op, F, FildexFile](Repair(bf, filler))
    def load(bf: BlockFile): Par[F, FildexFile]                       = liftPar_T[Op, F, FildexFile](Load(bf))
    def init(bf: BlockFile): Par[F, FildexFile]                       = liftPar_T[Op, F, FildexFile](Init(bf))
    def close(bf: FildexFile): Par[F, Unit]                           = liftPar_T[Op, F, Unit](Close(bf))
    def fetch(key: String, fildex: FildexFile): Par[F, Option[Idx]]   = liftPar_T[Op, F, Option[Idx]](Fetch(key, fildex))
    def query(limit: Int, order: Order, fildex: FildexFile): Par[F, Vector[Idx]] =
      liftPar_T[Op, F, Vector[Idx]](Query(limit, order, fildex))
    def append(bf: FildexFile, idx: Idx): Par[F, FildexFile] = liftPar_T[Op, F, FildexFile](Append(bf, idx))
  }
  implicit def to[F[_]](implicit I: Inject[Op, F]): Fildex[F] = new To[F]

  def apply[F[_]](implicit F: Fildex[F]) = F

  trait Handler[M[_]] extends NT[Op, M] {

    def check(bf: BlockFile, filler: FillerFile): M[Boolean]
    def repair(bf: BlockFile, filler: FillerFile): M[FildexFile]
    def load(bf: BlockFile): M[FildexFile]
    def init(bf: BlockFile): M[FildexFile]
    def append(bf: FildexFile, idx: Idx): M[FildexFile]
    def close(bf: FildexFile): M[Unit]
    def fetch(key: String, fildex: FildexFile): M[Option[Idx]]
    def query(limit: Int, order: Order, fildex: FildexFile): M[Vector[Idx]]

    def apply[A](fa: Op[A]): M[A] = fa match {
      case Check(bf, filler)           ⇒ check(bf, filler)
      case Load(bf)                    ⇒ load(bf)
      case Repair(bf, filler)          ⇒ repair(bf, filler)
      case Init(bf)                    ⇒ init(bf)
      case Close(bf)                   ⇒ close(bf)
      case Append(bf, idx)             ⇒ append(bf, idx)
      case Fetch(key, fildex)          ⇒ fetch(key, fildex)
      case Query(limit, order, fildex) ⇒ query(limit, order, fildex)
    }
  }

  trait FildexFile

  trait Data

  // Index structure
  final case class Idx(key: String, startPoint: Long, endPoint: Long) {
    require(key.getBytes("utf-8").length == 32, "fildex index key should be 32bit String")

    def byteBuffer: ByteBuffer = {
      val bb = ByteBuffer.allocate(48)
      bb.put(key.getBytes("utf-8"))
      bb.putLong(startPoint)
      bb.putLong(endPoint)
      bb.flip()
      bb
    }

    def fileLength: Long = endPoint - startPoint
  }

}
