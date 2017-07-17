package ufs3
package log
package interpreter

import cats.data.Kleisli
import cats.effect.IO
import org.apache.log4j.Logger
import ufs3.kernel.log.Log

/**
  * Created by songwenchao on 2017/7/17.
  */
trait LogInterpreter extends Log.Handler[Kleisli[IO, Unit, Unit]] {

  private lazy val logger: Logger = Logger.getLogger(getClass.getName)

  override def debug(msg: String, cause: Option[Throwable]): Kleisli[IO, Unit, Unit] = Kleisli { _ ⇒
    IO.pure {
      cause match {
        case Some(t) ⇒ logger.debug(msg, t)
        case None    ⇒ logger.debug(msg)
      }
    }
  }

  override def info(msg: String, cause: Option[Throwable]): Kleisli[IO, Unit, Unit] = Kleisli { _ ⇒
    IO.pure {
      cause match {
        case Some(t) ⇒ logger.info(msg, t)
        case None    ⇒ logger.info(msg)
      }
    }
  }

  override def warn(msg: String, cause: Option[Throwable]): Kleisli[IO, Unit, Unit] = Kleisli { _ ⇒
    IO.pure {
      cause match {
        case Some(t) ⇒ logger.warn(msg, t)
        case None    ⇒ logger.warn(msg)
      }
    }
  }

  override def error(msg: String, cause: Option[Throwable]): Kleisli[IO, Unit, Unit] = Kleisli { _ ⇒
    IO.pure {
      cause match {
        case Some(t) ⇒ logger.error(msg, t)
        case None    ⇒ logger.error(msg)
      }
    }
  }

}
