/**
  * command_backup_server.scala
  * ----------
  * backup-server command implementation
  * @author bigknife
  * @since 2017/7/26
  */
package ufs3
package integration
package command

import akka.actor.ActorSystem
import com.barcsys.tcp.connection.TcpConnector
import com.barcsys.tcp.connection.buffer.{BufferSettings, IOBufferSettings}
import com.barcsys.tcp.connection.visitor.{CloseConnectionVisitor, PangVisitor}
import com.typesafe.config.{Config, ConfigFactory}
import pharaoh.LogbackConfExtension
import ufs3.core.data.Data._
import ufs3.integration.command.backupserver.{BackupActor, BackupVisitor}

import scala.concurrent.Future
import scala.concurrent.duration._


trait BackupServerCommand {

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

  def run(coreConfig: CoreConfig, host: String, port: Int): Future[Unit] = {
    val server = TcpConnector.createServer(host, port)
    val f      = server.startup()
    //register some visitor
    server.registerConnectionVisitor(PangVisitor("PING", "PANG"))
    server.registerConnectionVisitor(CloseConnectionVisitor("quit", "exit"))
    server.registerConnectionVisitor(new BackupVisitor(coreConfig, BackupActor.backupActorRef(coreConfig)))
    f.map(_ ⇒ ())
  }
}

object BackupServerCommand extends BackupServerCommand
