/**
  * log.scala
  * ---------
  * the kernel adt of logging
  * @author: bigknife@outlook.com
  * @create: 2017/07/03
  */
package ufs3
package kernel
package log

import sop._
import scala.language.higherKinds
import cats.free.Inject

// log free monad
sealed trait Log[F[_]] {
  def debug(msg: String): Par[F, Unit]
  def info(msg: String): Par[F, Unit]
  def warn(msg: String): Par[F, Unit]
  def error(msg: String): Par[F, Unit]

  def error(msg: String, cause: Throwable): Par[F, Unit]
  def debug(msg: String, cause: Throwable): Par[F, Unit]
  def info(msg: String, cause: Throwable): Par[F, Unit]
  def warn(msg: String, cause: Throwable): Par[F, Unit]
}

object Log {
  sealed trait Op[A] extends Product with Serializable

  case class Debug(msg: String, cause: Option[Throwable] = None) extends Op[Unit]
  case class Info(msg: String, cause: Option[Throwable] = None)  extends Op[Unit]
  case class Warn(msg: String, cause: Option[Throwable] = None)  extends Op[Unit]
  case class Error(msg: String, cause: Option[Throwable] = None) extends Op[Unit]

  // Ops to lift to Free Monad
  class To[F[_]](implicit I: Inject[Op, F]) extends Log[F] {
    def debug(msg: String): Par[F, Unit] = liftPar_T[Op, F, Unit](Debug(msg))
    def info(msg: String): Par[F, Unit]  = liftPar_T[Op, F, Unit](Info(msg))
    def warn(msg: String): Par[F, Unit]  = liftPar_T[Op, F, Unit](Warn(msg))
    def error(msg: String): Par[F, Unit] = liftPar_T[Op, F, Unit](Error(msg))

    def error(msg: String, cause: Throwable): Par[F, Unit] = liftPar_T[Op, F, Unit](Error(msg, Some(cause)))
    def debug(msg: String, cause: Throwable): Par[F, Unit] = liftPar_T[Op, F, Unit](Debug(msg, Some(cause)))
    def info(msg: String, cause: Throwable): Par[F, Unit]  = liftPar_T[Op, F, Unit](Info(msg, Some(cause)))
    def warn(msg: String, cause: Throwable): Par[F, Unit]  = liftPar_T[Op, F, Unit](Warn(msg, Some(cause)))
  }
  implicit def to[F[_]](implicit I: Inject[Op, F]): To[F] = new To[F]
  def apply[F[_]](implicit L: Log[F]): Log[F] = L

  trait Handler[M[_]] extends NT[Op, M] {
    protected[this] def debug(msg: String, cause: Option[Throwable]): M[Unit]
    protected[this] def info(msg: String, cause: Option[Throwable]): M[Unit]
    protected[this] def warn(msg: String, cause: Option[Throwable]): M[Unit]
    protected[this] def error(msg: String, cause: Option[Throwable]): M[Unit]

    override def apply[A](fa: Op[A]): M[A] = fa match {
      case Debug(msg, cause) ⇒ debug(msg, cause)
      case Info(msg, cause)  ⇒ info(msg, cause)
      case Warn(msg, cause)  ⇒ warn(msg, cause)
      case Error(msg, cause) ⇒ error(msg, cause)
    }
  }
}
