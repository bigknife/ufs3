/**
  * backup.scala
  * ------------
  * the kernel adt of backup
  * @author: bigknife@outlook.com
  * @create: 2017/07/03
  */
package ufs3
package kernel
package backup

import scala.language.higherKinds
import scala.language.implicitConversions

import cats.free.{Free, Inject}
import Free._

/**
  * Backup
  * ------
  * UFS3 Backup adt
  */
sealed trait Backup[A]
object Backup {
  type Response[A] = Either[Throwable, A]

  case object Open  extends Backup[Response[Unit]]
  case object Close extends Backup[Response[Unit]]

  case class Send(data: Data) extends Backup[Response[Unit]]

  class Ops[F[_]](implicit I: Inject[Backup, F]) {
    def open(): Free[F, Response[Unit]]  = inject[Backup, F](Open)
    def close(): Free[F, Response[Unit]] = inject[Backup, F](Close)

    def send(data: Data): Free[F, Response[Unit]] = inject[Backup, F](Send(address, data))
  }
  object Ops {
    implicit def ops[F[_]](implicit I: Inject[Backup, F]) = new Ops[F]
  }
}

/**
  * Data
  * ----
  * Backup Data
  */
sealed trait Data
