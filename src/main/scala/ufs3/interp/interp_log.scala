package ufs3.interp

import org.slf4j.{Logger, LoggerFactory}
import ufs3.interp.commons.Stack
import ufs3.kernel.algebras.Log
import ufs3.kernel.commons._

import scala.language.implicitConversions

object log {

  trait Handler extends Log.Handler[Stack] {
    private lazy val logger: Logger = LoggerFactory.getLogger("ast")

    protected[this] def error(msg: String, cause: Option[Throwable]): Stack[Unit] = Stack.configless[Unit] {
      cause match {
        case Some(t) ⇒ logger.error(msg, t)
        case None    ⇒ logger.error(msg)
      }
    }

    protected[this] def debug(msg: String, cause: Option[Throwable]): Stack[Unit] = Stack.configless[Unit] {
      cause match {
        case Some(t) ⇒ logger.debug(msg, t)
        case None    ⇒ logger.debug(msg)
      }
    }

    protected[this] def info(msg: String, cause: Option[Throwable]): Stack[Unit] = Stack.configless[Unit] {
      cause match {
        case Some(t) ⇒ logger.info(msg, t)
        case None    ⇒ logger.info(msg)
      }
    }

    protected[this] def warn(msg: String, cause: Option[Throwable]): Stack[Unit] = Stack.configless[Unit] {
      cause match {
        case Some(t) ⇒ logger.warn(msg, t)
        case None    ⇒ logger.warn(msg)
      }
    }
  }

  implicit object Handler extends Handler
}
