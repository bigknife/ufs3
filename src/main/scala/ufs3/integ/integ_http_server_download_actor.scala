package ufs3.integ.http_server.actor

import java.io.{InputStream, OutputStream}
import akka.actor.{Actor, Props}
import org.slf4j.{Logger, LoggerFactory}
import ufs3.integ.httpServer
import ufs3.interp.commons.Stack
import ufs3.kernel.commons.Config
import ufs3.kernel.modules.App
import ufs3.prog._

import scala.util.{Failure, Success, Try}

object DownloadLogger {
  val logger: Logger = LoggerFactory.getLogger("download")
}
class DownloadActor extends Actor {
  import DownloadLogger.logger._
  def receive: Receive = {
    case DownloadActor.Download(config, key, out, in) ⇒
      val ufs3 = httpServer.openUFS3(config)
      val p    = read[App.Op](key, ufs3, out)

      Try {
        val task = Stack.parseApp(p).run(config)
        task.unsafeRun()
      } match {
        case Success(_) ⇒
          info("GetCommand succeeded")
          out.close()
        case Failure(t) ⇒
          error("GetCommand failed", t)
          out.write(t.getMessage.getBytes("utf-8"))
          out.close()
      }
  }
}
object DownloadActor {
  def props: Props = Props(classOf[DownloadActor])



  sealed trait Command
  final case class Download(config: Config, key: String, out: OutputStream, in: InputStream) extends Command
}
