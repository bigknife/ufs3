/**
  * backup.scala
  * ----------
  * interpreter of backup
  * @author bigknife
  * @since 2017/7/17
  */
package ufs3
package interpreter
package backup

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicReference

import ufs3.kernel.backup._
import Backup._
import cats.data.Kleisli
import cats.effect.IO
import BackupInterpreter._
import akka.actor.ActorSystem
import akka.util.ByteString
import cats.Eval
import com.barcsys.tcp.connection.{Connection, TcpConnector}
import com.barcsys.tcp.connection.buffer.{BufferSettings, IOBufferSettings}
import com.barcsys.tcp.connection.connector.ClientConnector
import com.barcsys.tcp.connection.visitor.{AutoReconnectVisitor, ConnectionWatcher, PangVisitor}
import sop.Resp

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.concurrent.duration.Duration

trait BackupInterpreter extends Handler[Kleisli[IO, Config, ?]] {
  /*
  private[this] val connector  = new AtomicReference[TcpConnector[Connection]]()
  private[this] val connection = new AtomicReference[Connection]()
  private[this] val client     = new AtomicReference[ClientConnector]()

  def open(): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    import config.actorSystem, config.idle, config.bufSettings, config.ec
    IO.fromFuture[Resp[Unit]](Eval.later {
      val _client =
        TcpConnector.createClient(
          targets = Vector(new InetSocketAddress(config.backupHost, config.backupPort)),
          AutoReconnectVisitor(config.autoReconnectIdle),
          PangVisitor(ping = "!--hi-->", pang = "<--hi--!"),
          ConnectionWatcher(
            cbConnected = (_connector, _connection) ⇒ {
              connector.set(_connector)
              connection.set(_connection)
            }
          )
        )
      client.set(_client)
      val f: Future[Resp[Unit]] = _client
        .startup()
        .map(x ⇒ {
          connector.set(x._1)
          connection.set(x._2.get)
        })
      f
    })
  }

  def close(): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    import config._
    IO.fromFuture[Unit](Eval.later {
      if (client.get() != null) client.get.shutdown().map(_ ⇒ ())
      else Future.successful[Unit](())
    })
  }

  def send(data: ByteBuffer): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    import java.io.IOException
    import config._
    IO.fromFuture[Unit](Eval.later {
      if (connector.get() != null)
        connector
          .get()
          .write(ByteString(data), Option(connection.get()))
          .flatMap(writed ⇒
            if (writed) Future.successful[Unit](())
            else Future.failed[Unit](new IOException(s"can't write to ${Option(connection.get())}")))
      else
        Future.failed[Unit](new IOException(s"connector to ${config.backupHost}:${config.backupPort} not opened now"))
    })
  }
  */
}

object BackupInterpreter {

  trait Config {
    def backupHost: String
    def backupPort: Int
    def autoReconnectIdle: Duration            = 3.seconds
    implicit def ec: ExecutionContext          = scala.concurrent.ExecutionContext.Implicits.global
    implicit def actorSystem: ActorSystem      = ActorSystem()
    implicit def idle: Duration                = 30.seconds
    implicit def bufSettings: IOBufferSettings = IOBufferSettings(BufferSettings.Default, BufferSettings.Default)

  }

  def config(_backupHost: String, _backupPort: Int): Config = new Config {
    def backupHost: String = _backupHost
    def backupPort: Int    = _backupPort
  }
  /*
    def apply(): BackupInterpreter = new BackupInterpreter() {}
    */
}
