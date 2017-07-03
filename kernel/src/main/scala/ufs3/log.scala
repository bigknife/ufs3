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

// log free monad
sealed trait Log[A]

object Log {
  case class Debug(msg: String, cause: Option[Throwable] = None) extends Log[Unit]
  case class Info(msg: String, cause: Option[Throwable] = None) extends Log[Unit]
  case class Warn(msg: String, cause: Option[Throwable] = None) extends Log[Unit]
  case class Error(msg: String, cause: Option[Throwable] = None) extends Log[Unit]

  // Ops to lift to Free Monad
  import Free._

  class Ops[F[_]](implicit I: Inject[Log, F]) {
    def debug(msg: String): Free[F, Unit] = inject[Log, F](Debug(msg))
    def debug(msg: String, cause: Throwable): Free[F, Unit] = inject[Log, F](Debug(msg, Some(cause)))
    def info(msg: String): Free[F, Unit] = inject[Log, F](Info(msg))
    def info(msg: String, cause: Throwable): Free[F, Unit] = inject[Log, F](Info(msg, Some(cause)))
    def warn(msg: String): Free[F, Unit] = inject[Log, F](Warn(msg))
    def warn(msg: String, cause: Throwable): Free[F, Unit] = inject[Log, F](Warn(msg, Some(cause)))
    def error(msg: String): Free[F, Unit] = inject[Log, F](Error(msg))
    def error(msg: String, cause: Throwable): Free[F, Unit] = inject[Log, F](Error(msg, Some(cause)))
  }
  object Ops {
    implicit def toOps[F[_]](implicit I: Inject[Log, F]): Ops[F] = new Ops[F]
  }
}
