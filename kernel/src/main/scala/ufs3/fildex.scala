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
import ufs3.kernel.filler.Filler.FillerFile

import scala.language.{higherKinds, implicitConversions}

trait Fildex[F[_]] {
  import Fildex._

  def check(ff: FillerFile): Par[F, FildexFile]
  def repair(ff: FillerFile): Par[F, Unit]
  def create(ff: FillerFile): Par[F, FildexFile]
  def append(ff: FildexFile, idx: Idx): Par[F, Unit]
  def close(ff: FildexFile): Par[F, Unit]
  def fetch(key: String): Par[F, Option[Idx]]
}

object Fildex {
  sealed trait Op[A]
  final case class Check(ff: FillerFile)  extends Op[FildexFile]
  final case class Repair(ff: FillerFile) extends Op[Unit]
  final case class Create(ff: FillerFile) extends Op[FildexFile]
  final case class Close(ff: FildexFile)  extends Op[Unit]
  final case class Fetch(key: String)     extends Op[Option[Idx]]

  final case class Append(ff: FildexFile, idx: Idx) extends Op[Unit]

  class To[F[_]](implicit I: Inject[Op, F]) extends Fildex[F] {
    def check(ff: FillerFile): Par[F, FildexFile]      = liftPar_T[Op, F, FildexFile](Check(ff))
    def repair(ff: FillerFile): Par[F, Unit]           = liftPar_T[Op, F, Unit](Repair(ff))
    def create(ff: FillerFile): Par[F, FildexFile]     = liftPar_T[Op, F, FildexFile](Create(ff))
    def close(ff: FildexFile): Par[F, Unit]            = liftPar_T[Op, F, Unit](Close(ff))
    def fetch(key: String): Par[F, Option[Idx]]        = liftPar_T[Op, F, Option[Idx]](Fetch(key))
    def append(ff: FildexFile, idx: Idx): Par[F, Unit] = liftPar_T[Op, F, Unit](Append(ff, idx))
  }
  implicit def to[F[_]](implicit I: Inject[Op, F]): Fildex[F] = new To[F]

  def apply[F[_]](implicit F: Fildex[F]) = F

  trait Handler[M[_]] extends NT[Op, M] {

    def check(ff: FillerFile): M[FildexFile]
    def repair(ff: FillerFile): M[Unit]
    def create(ff: FillerFile): M[FildexFile]
    def append(ff: FildexFile, idx: Idx): M[Unit]
    def close(ff: FildexFile): M[Unit]
    def fetch(key: String): M[Option[Idx]]

    def apply[A](fa: Op[A]): M[A] = fa match {
      case Check(ff)       ⇒ check(ff)
      case Repair(ff)      ⇒ repair(ff)
      case Create(ff)      ⇒ create(ff)
      case Close(ff)       ⇒ close(ff)
      case Append(ff, idx) ⇒ append(ff, idx)
      case Fetch(key)      ⇒ fetch(key)
    }
  }

  trait FildexFile {
    def hostFiller: FillerFile
    def writeData(data: ByteBuffer, size: Long): Unit
    def seekToTail(): Unit
    def close(): Unit
    protected var tailPosition: Long = 0L
  }
  object FildexFile {
    private[this] final class RandomAccessFildexFile(private val underlying: RandomAccessFile,
                                                     private val host: FillerFile)
        extends FildexFile {
      override def hostFiller: FillerFile = host

      override def writeData(data: ByteBuffer, size: Long): Unit = {
        val channel = underlying.getChannel
        channel.map(FileChannel.MapMode.READ_WRITE, channel.position(), size).put(data)
        tailPosition += size
      }

      override def seekToTail(): Unit = underlying.seek(tailPosition)

      override def close(): Unit = underlying.close()
    }

    def apply(hostFiller: FillerFile): Eval[FildexFile] =
      Eval.later(new RandomAccessFildexFile(new RandomAccessFile(s"${hostFiller.path}.index", "rw"), hostFiller))
  }

  trait Data

  // Index structure
  final case class Idx(key: String, startPoint: Long, endPoint: Long)
}
