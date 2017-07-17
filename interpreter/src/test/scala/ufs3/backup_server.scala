package ufs3
package interpreter
package backup

import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicReference

import akka.util.ByteString
import com.barcsys.tcp.connection.connector.ServerConnector
import com.barcsys.tcp.connection.visitor.{AbstractConnectionVisitor, PingVisitor}
import com.barcsys.tcp.connection.{Connection, TcpConnector}
import org.scalatest.FlatSpec

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

trait BackupMockServer {

  val server = new AtomicReference[ServerConnector]()
  def start(config: BackupInterpreter.Config): Future[(TcpConnector[Connection], Option[Connection])] = {
    import config.actorSystem, config.ec, config.idle, config.bufSettings
    val _server = TcpConnector.createServer(config.backupHost, config.backupPort)
    server.set(_server)
    val f = _server.startup()
    f.onComplete {
      case Success(x) ⇒ println(s"server startuped, listen: $x")
      case Failure(t) ⇒ t.printStackTrace()
    }
    _server.registerConnectionVisitor(PingVisitor("!--hi-->"))
    _server.registerConnectionVisitor(new AbstractConnectionVisitor {
      override def onRead(connector: TcpConnector[Connection], connection: Connection, data: ByteString): ByteString = {
        println(s"GET: ${data.decodeString("utf-8")}")
        ByteString.empty
      }
    })
    f
  }

  def stop(): Unit = server.get().shutdown(); ()
}

object BackupMockServer extends BackupMockServer

/**
  * Backup spec
  */
class BackupSpec extends FlatSpec {
  lazy val config: BackupInterpreter.Config = BackupInterpreter.config("localhost", 3800)
  "Backup" should "send the same data to backup server" in {
    info("start backup mock server")
    Await.result(BackupMockServer.start(config), Duration.Inf)
    Thread.sleep(1000)

    val backupInterpreter = BackupInterpreter()
    val program = for {
      _ ← backupInterpreter.open()
      _ ← backupInterpreter.send(ByteBuffer.wrap("This is a simple test 1".getBytes))
      _ ← backupInterpreter.send(ByteBuffer.wrap("This is a simple test 2".getBytes))
      _ ← backupInterpreter.send(ByteBuffer.wrap("This is a simple test 3".getBytes))
      _ ← backupInterpreter.send(ByteBuffer.wrap("This is a simple test 4".getBytes))
      _ ← backupInterpreter.close()
    } yield ()

    program.run(config).unsafeRunSync()


    info("stop backup mock server")
    BackupMockServer.stop()
    config.actorSystem.shutdown()
  }
}