/**
  * backup.scala
  * ------------
  * the kernel adt of backup
  * @author : bigknife@outlook.com
  * @since : 2017/07/03
  */
package ufs3
package kernel
package backup

import java.nio.ByteBuffer

import scala.language.higherKinds
import scala.language.implicitConversions
import cats.free.Inject
import sop._

/**
  * Backup
  * ------
  * UFS3 Backup adt
  */
sealed trait Backup[F[_]] {
  def open(): RespPar[F, Unit]
  def close(): RespPar[F, Unit]

  //def startBackup(key: String): RespPar[F, Unit]
  def backupData(data: ByteBuffer): RespPar[F, Unit]
  //def endBackup(key: String, startPos: Long, endPos: Long): RespPar[F, Unit]
}
object Backup {
  sealed trait Op[A]
  case object Open                  extends Op[Resp[Unit]]
  case object Close                 extends Op[Resp[Unit]]
  case class Send(data: ByteBuffer) extends Op[Resp[Unit]]

  class To[F[_]](implicit I: Inject[Op, F]) extends Backup[F] {
    override def open(): RespPar[F, Unit]                 = liftPar_T[Op, F, Resp[Unit]](Open)
    override def close(): RespPar[F, Unit]                = liftPar_T[Op, F, Resp[Unit]](Close)
    override def backupData(data: ByteBuffer): RespPar[F, Unit] = liftPar_T[Op, F, Resp[Unit]](Send(data))
  }

  implicit def to[F[_]](implicit I: Inject[Op, F])  = new To[F]
  def apply[F[_]](implicit B: Backup[F]): Backup[F] = B

  trait Handler[M[_]] extends NT[Op, M] {
    def open(): M[Resp[Unit]]
    def close(): M[Resp[Unit]]
    def send(data: ByteBuffer): M[Resp[Unit]]

    override def apply[A](fa: Op[A]): M[A] = fa match {
      case Open       ⇒ open()
      case Close      ⇒ close()
      case Send(data) ⇒ send(data)
    }
  }
}
