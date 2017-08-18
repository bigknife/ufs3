package ufs3.integ

import fs2.Task
import ufs3.world.commons.BackupServerArg
import akka.actor.ActorSystem
import com.barcsys.tcp.connection.TcpConnector
import com.barcsys.tcp.connection.buffer.{BufferSettings, IOBufferSettings}
import com.barcsys.tcp.connection.visitor.{CloseConnectionVisitor, PangVisitor}
import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.{Logger, LoggerFactory}
import pharaoh.LogbackConfExtension
import ufs3.integ.backup_server.visitor.{BackupActor, BackupVisitor}

import scala.concurrent.duration._
import scala.util.{Failure, Success}

object backupServer {
  lazy val log: Logger = LoggerFactory.getLogger("backupServer")

  def config(): Config = ConfigFactory.parseResources("META-INF/application.conf").withFallback(ConfigFactory.load())

  implicit val system: ActorSystem = {
    val sys = ActorSystem("BackupServer", config())
    // logback extension
    LogbackConfExtension(sys)
    sys
  }
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val idle: Duration                = 1.minute
  implicit val bufSettings: IOBufferSettings = IOBufferSettings(BufferSettings.Default, BufferSettings.Default)

  def apply(config: BackupServerArg): Task[Unit] = {

    Task.delay {
      val server = TcpConnector.createServer(config.host, config.port)
      val f      = server.startup()
      //register some visitor
      server.registerConnectionVisitor(PangVisitor("PING\r\n", "PANG\r\n"))
      server.registerConnectionVisitor(CloseConnectionVisitor("quit", "exit"))
      server.registerConnectionVisitor(new BackupVisitor(config.asConfig, BackupActor.backupActorRef(config.asConfig)))
      f.onComplete {
        case Success(_) ⇒ log.info(s"start backup server:${config.host}:${config.port}")
        case Failure(t) ⇒ log.error("start backup server failed", t)
      }
    }
  }
}