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
import akka.util.ByteString
import com.barcsys.tcp.connection.{Connection, TcpConnector}
import com.barcsys.tcp.connection.buffer.{BufferSettings, IOBufferSettings}
import com.barcsys.tcp.connection.visitor.{AbstractConnectionVisitor, CloseConnectionVisitor, PangVisitor}
import com.typesafe.config.{Config, ConfigFactory}
import pharaoh.LogbackConfExtension

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
  implicit val idle: Duration = 1.minute
  implicit val bufSettings: IOBufferSettings = IOBufferSettings(BufferSettings.Default, BufferSettings.Default)

  def run(host: String, port: Int): Future[Unit] = {
    val server = TcpConnector.createServer(host, port)
    val f = server.startup()
    //register some visitor
    server.registerConnectionVisitor(PangVisitor("PING", "PANG"))
    server.registerConnectionVisitor(CloseConnectionVisitor("quit", "exit"))
    server.registerConnectionVisitor(new BackupVisitor)
    f.map(_ ⇒ ())
  }

  class BackupVisitor extends AbstractConnectionVisitor {
    override def onRead(connector: TcpConnector[Connection], connection: Connection, data: ByteString): ByteString = {
      println(data.utf8String)
      ByteString.empty
    }
  }
}

object BackupServerCommand extends BackupServerCommand