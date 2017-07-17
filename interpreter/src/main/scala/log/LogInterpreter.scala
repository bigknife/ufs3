package log

import org.apache.log4j.Logger
import ufs3.kernel.log.Log

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by songwenchao on 2017/7/5.
  */
object LogInterpreter extends Log.Handler[Future] {

  private lazy val logger: Logger = Logger.getLogger(getClass.getName)

  override protected[this] def debug(msg: String, cause: Option[Throwable]): Future[Unit] = Future {
    cause match {
      case Some(x) ⇒ logger.debug(msg, x)
      case None    ⇒ logger.debug(msg)
    }
  }

  override protected[this] def info(msg: String, cause: Option[Throwable]): Future[Unit] = Future {
    cause match {
      case Some(x) ⇒ logger.info(msg, x)
      case None    ⇒ logger.info(msg)
    }
  }

  override protected[this] def warn(msg: String, cause: Option[Throwable]): Future[Unit] = Future {
    cause match {
      case Some(x) ⇒ logger.warn(msg, x)
      case None    ⇒ logger.warn(msg)
    }
  }

  override protected[this] def error(msg: String, cause: Option[Throwable]): Future[Unit] = Future {
    cause match {
      case Some(x) ⇒ logger.error(msg, x)
      case None    ⇒ logger.error(msg)
    }
  }
}
