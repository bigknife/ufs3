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
  def debug(msg: String): RespPar[F, Unit]
  def info(msg: String): RespPar[F, Unit]
  def warn(msg: String): RespPar[F, Unit]
  def error(msg: String): RespPar[F, Unit]

  def error(msg: String, cause: Throwable): RespPar[F, Unit]
  def debug(msg: String, cause: Throwable): RespPar[F, Unit]
  def info(msg: String, cause: Throwable): RespPar[F, Unit]
  def warn(msg: String, cause: Throwable): RespPar[F, Unit]
}

object Log {
  sealed trait Op[A] extends Product with Serializable

  case class Debug(msg: String, cause: Option[Throwable] = None) extends Op[Resp[Unit]]
  case class Info(msg: String, cause: Option[Throwable] = None)  extends Op[Resp[Unit]]
  case class Warn(msg: String, cause: Option[Throwable] = None)  extends Op[Resp[Unit]]
  case class Error(msg: String, cause: Option[Throwable] = None) extends Op[Resp[Unit]]

  // Ops to lift to Free Monad
  class To[F[_]](implicit I: Inject[Op, F]) extends Log[F] {
    def debug(msg: String): RespPar[F, Unit] = liftPar_T[Op, F, Resp[Unit]](Debug(msg))
    def info(msg: String): RespPar[F, Unit]  = liftPar_T[Op, F, Resp[Unit]](Info(msg))
    def warn(msg: String): RespPar[F, Unit]  = liftPar_T[Op, F, Resp[Unit]](Warn(msg))
    def error(msg: String): RespPar[F, Unit] = liftPar_T[Op, F, Resp[Unit]](Error(msg))

    def error(msg: String, cause: Throwable): RespPar[F, Unit] = liftPar_T[Op, F, Resp[Unit]](Error(msg, Some(cause)))
    def debug(msg: String, cause: Throwable): RespPar[F, Unit] = liftPar_T[Op, F, Resp[Unit]](Debug(msg, Some(cause)))
    def info(msg: String, cause: Throwable): RespPar[F, Unit]  = liftPar_T[Op, F, Resp[Unit]](Info(msg, Some(cause)))
    def warn(msg: String, cause: Throwable): RespPar[F, Unit]  = liftPar_T[Op, F, Resp[Unit]](Warn(msg, Some(cause)))
  }
  implicit def to[F[_]](implicit I: Inject[Op, F]): To[F] = new To[F]
  def apply[F[_]](implicit L: Log[F]): Log[F]             = L

  trait Handler[M[_]] extends NT[Op, M] {
    protected[this] def debug(msg: String, cause: Option[Throwable]): M[Resp[Unit]]
    protected[this] def info(msg: String, cause: Option[Throwable]): M[Resp[Unit]]
    protected[this] def warn(msg: String, cause: Option[Throwable]): M[Resp[Unit]]
    protected[this] def error(msg: String, cause: Option[Throwable]): M[Resp[Unit]]

    override def apply[A](fa: Op[A]): M[A] = fa match {
      case Debug(msg, cause) ⇒ debug(msg, cause)
      case Info(msg, cause)  ⇒ info(msg, cause)
      case Warn(msg, cause)  ⇒ warn(msg, cause)
      case Error(msg, cause) ⇒ error(msg, cause)
    }
  }
}
