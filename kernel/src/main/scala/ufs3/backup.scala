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

import scala.language.higherKinds
import scala.language.implicitConversions

import cats.free.{Free, Inject}
import Free._
import sop._

/**
  * Backup
  * ------
  * UFS3 Backup adt
  */
sealed trait Backup[F[_]] {
  def open(): Par[F, Unit]
  def close(): Par[F, Unit]
  def send(data: Data): Par[F, Unit]
}
object Backup {
  sealed trait Op[A]
  case object Open            extends Op[Unit]
  case object Close           extends Op[Unit]
  case class Send(data: Data) extends Op[Unit]

  class To[F[_]](implicit I: Inject[Op, F]) extends Backup[F] {
    override def open(): Par[F, Unit]           = liftPar_T[Op, F, Unit](Open)
    override def close(): Par[F, Unit]          = liftPar_T[Op, F, Unit](Close)
    override def send(data: Data): Par[F, Unit] = liftPar_T[Op, F, Unit](Send(data))
  }

  implicit def to[F[_]](implicit I: Inject[Op, F]) = new To[F]
  def apply[F[_]](implicit B: Backup[F]): Backup[F]    = B

}

/**
  * Data
  * ----
  * Backup Data
  */
sealed trait Data
