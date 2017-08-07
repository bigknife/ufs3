package ufs3
package log
package interpreter

import cats.data.Kleisli
import cats.effect.IO
import org.apache.log4j.Logger
import sop.Resp
import ufs3.kernel.log.Log
import ufs3.log.interpreter.LogInterpreter.Config

/**
  * Created by songwenchao on 2017/7/17.
  */
trait LogInterpreter extends Log.Handler[Kleisli[IO, Config, ?]] {

  private lazy val logger: Logger = Logger.getLogger("ufs3")

  override def debug(msg: String, cause: Option[Throwable]): Kleisli[IO, Config, Resp[Unit]] = Kleisli { config ⇒
    IO {
        cause match {
          case Some(t) ⇒ logger.debug(msg, t)
          case None    ⇒ logger.debug(msg)
        }
    }.attempt
  }

  override def info(msg: String, cause: Option[Throwable]): Kleisli[IO, Config, Resp[Unit]] = Kleisli { config ⇒
    IO {
        cause match {
          case Some(t) ⇒ logger.info(msg, t)
          case None    ⇒ logger.info(msg)
        }
    }.attempt
  }

  override def warn(msg: String, cause: Option[Throwable]): Kleisli[IO, Config, Resp[Unit]] = Kleisli { config ⇒
    IO {
      cause match {
        case Some(t) ⇒ logger.warn(msg, t)
        case None    ⇒ logger.warn(msg)
      }
    }.attempt
  }

  override def error(msg: String, cause: Option[Throwable]): Kleisli[IO, Config, Resp[Unit]] = Kleisli { config ⇒
    IO {
      cause match {
        case Some(t) ⇒ logger.error(msg, t)
        case None    ⇒ logger.error(msg)
      }
    }.attempt
  }

}

object LogInterpreter {

  trait Config

  def config(): Config = new Config {}

  def apply(): LogInterpreter = new LogInterpreter {}
}
