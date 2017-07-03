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

  case class StartServing(address: BackupAddress) extends Backup[Response[Unit]]
  case object Find extends Backup[Response[BackupAddress]]
  case class Open(address: BackupAddress) extends Backup[Response[Unit]]
  case class Send(address: BackupAddress, data: Data) extends Backup[Response[Unit]]
  case class Close(address: BackupAddress) extends Backup[Response[Unit]]
  case class StopServing(address: BackupAddress) extends Backup[Response[Unit]]

  class Ops[F[_]](implicit I: Inject[Backup, F]) {
    def startServing(address: BackupAddress): Free[F, Response[Unit]] = inject[Backup, F](StartServing(address))
    def find(): Free[F, Response[BackupAddress]] = inject[Backup, F](Find)
    def open(address: BackupAddress): Free[F, Response[Unit]] = inject[Backup, F](Open(address))
    def send(address: BackupAddress, data: Data): Free[F, Response[Unit]] = inject[Backup, F](Send(address, data))
    def close(address: BackupAddress): Free[F, Response[Unit]] = inject[Backup, F](Close(address))
    def stopServing(address: BackupAddress): Free[F, Response[Unit]] = inject[Backup, F](StopServing(address))
  }

  implicit def ops[F[_]](implicit I: Inject[Backup, F]) = new Ops[F]
}

/**
  * BackupAddress
  * -------------
  * backup server listening address
  */
sealed trait BackupAddress {
  def host: String
  def port: Int
}

object BackupAddress {
  def apply(h: String, p: Int): BackupAddress = new BackupAddress {
    val host: String = h
    val port: Int = p
  }
}

/**
  * Data
  * ----
  * Backup Data
  */
sealed trait Data
