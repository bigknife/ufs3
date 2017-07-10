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

import scala.language.higherKinds
import cats.free.{Free, Inject}
import cats.~>

// log free monad
sealed trait Log[F[_]] {
  def debug(msg: String): Free[F, Unit]
  def debug(msg: String, cause: Throwable): Free[F, Unit]
  def info(msg: String): Free[F, Unit]
  def info(msg: String, cause: Throwable): Free[F, Unit]
  def warn(msg: String): Free[F, Unit]
  def warn(msg: String, cause: Throwable): Free[F, Unit]
  def error(msg: String): Free[F, Unit]
  def error(msg: String, cause: Throwable): Free[F, Unit]
}

object Log {
  sealed trait Op[A]                                             extends Product with Serializable
  case class Debug(msg: String, cause: Option[Throwable] = None) extends Op[Unit]
  case class Info(msg: String, cause: Option[Throwable] = None)  extends Op[Unit]
  case class Warn(msg: String, cause: Option[Throwable] = None)  extends Op[Unit]
  case class Error(msg: String, cause: Option[Throwable] = None) extends Op[Unit]

  // Ops to lift to Free Monad
  import Free._

  class Ops[F[_]](implicit I: Inject[Op, F]) extends Log[F] {
    private[this] def inj[A](op: Op[A]): Free[F, A] = inject[Op, F](op)

    def debug(msg: String): Free[F, Unit]                   = inj(Debug(msg))
    def debug(msg: String, cause: Throwable): Free[F, Unit] = inj(Debug(msg, Some(cause)))
    def info(msg: String): Free[F, Unit]                    = inj(Info(msg))
    def info(msg: String, cause: Throwable): Free[F, Unit]  = inj(Info(msg, Some(cause)))
    def warn(msg: String): Free[F, Unit]                    = inj(Warn(msg))
    def warn(msg: String, cause: Throwable): Free[F, Unit]  = inj(Warn(msg, Some(cause)))
    def error(msg: String): Free[F, Unit]                   = inj(Error(msg))
    def error(msg: String, cause: Throwable): Free[F, Unit] = inj(Error(msg, Some(cause)))
  }
  implicit def toOps[F[_]](implicit I: Inject[Op, F]): Ops[F] = new Ops[F]

  def apply[F[_]](implicit L: Log[F]): Log[F] = L

  trait Handler[M[_]] extends (Op ~> M) {
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
