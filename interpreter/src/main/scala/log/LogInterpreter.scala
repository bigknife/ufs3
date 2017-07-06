package log

import cats.{Id, ~>}
import org.apache.log4j.Logger
import ufs3.kernel.log.Log
import ufs3.kernel.log.Log.{Debug, Error, Info, Warn}

/**
  * Created by songwenchao on 2017/7/5.
  */
object LogInterpreter {

  private lazy val logger: Logger = Logger.getLogger(getClass.getName)

  val logInterpreter: Log ~> Id = new (Log ~> Id) {
    override def apply[A](fa: Log[A]): Id[A] = fa match {
      case Debug(msg, cause) ⇒
        cause match {
          case Some(t) ⇒ logger.debug(msg, t)
          case None    ⇒ logger.debug(msg)
        }
      case Info(msg, cause) ⇒
        cause match {
          case Some(t) ⇒ logger.info(msg, t)
          case None    ⇒ logger.info(msg)
        }
      case Warn(msg, cause) ⇒
        cause match {
          case Some(t) ⇒ logger.warn(msg, t)
          case None    ⇒ logger.warn(msg)
        }
      case Error(msg, cause) ⇒
        cause match {
          case Some(t) ⇒ logger.error(msg, t)
          case None    ⇒ logger.error(msg)
        }
    }
  }
}
