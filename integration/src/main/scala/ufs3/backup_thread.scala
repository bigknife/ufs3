/**
  * backup_thread.scala
  * ----------
  * backup single thread
  * @author bigknife
  * @since 2017/7/31
  */
package ufs3
package integration
package command

import java.io.OutputStream
import java.net.InetSocketAddress
import java.security.MessageDigest
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorSystem
import akka.util.ByteString
import com.barcsys.tcp.connection.buffer.{BufferSettings, IOBufferSettings}
import com.barcsys.tcp.connection.visitor.{AbstractConnectionVisitor, AutoReconnectVisitor, PingVisitor}
import com.barcsys.tcp.connection.{Connection, TcpConnector}
import com.typesafe.config.{Config, ConfigFactory}
import org.apache.log4j.Logger
import pharaoh.LogbackConfExtension
import ufs3.core.data.Data
import ufs3.core.data.Data.CoreConfig
import ufs3.interpreter.layout.{SandwichHeadLayout, SandwichTailLayout}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

trait BackupSingleThread {
  val logger: Logger = Logger.getLogger("backup-thread")

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

  val coreConfig: CoreConfig
  val targetAddress: Option[InetSocketAddress]

  val tcpConnector: AtomicReference[Option[TcpConnector[Connection]]] =
    new AtomicReference[Option[TcpConnector[Connection]]](None)
  val tcpConnection: AtomicReference[Option[Connection]] = new AtomicReference[Option[Connection]](None)

  case class BackupEvent(key: String)

  // a bounded linked list blocking queue
  // max is Integer.MAX
  private val queue = new LinkedBlockingQueue[BackupEvent]()

  lazy val thread = new Thread(new Runnable {
    def run(): Unit = {
      handleQueue(queue)
    }
    @tailrec
    private def handleQueue(queue: LinkedBlockingQueue[BackupEvent]): Unit = {
      val event = queue.take()
      try {
        workFor(event)
      } catch {
        case t: Throwable ⇒ logger.error(s"work for $event failed", t)
      }

      handleQueue(queue)
    }
    private def workFor(event: BackupEvent): Unit = {
      logger.debug(s"Got backup event: $event")
      //todo backup
      val _connector = tcpConnector.get()
      val _connection = tcpConnection.get()
      if (_connector.isDefined) {
        val md5 = MessageDigest.getInstance("md5")
        GetCommand._idxOfKey(coreConfig, event.key).map {
          case Some(idx) ⇒
            // 1. Magic: "SAND"
            // 2. BODY_LENGTH, 8 (3#32 + 4#bodylength)
            // 3. KEY, 32
            // 4. body stream
            val bodyLength = 32 + idx.endPoint - idx.startPoint - SandwichHeadLayout.HEAD_LENGTH - SandwichTailLayout.TAIL_LENGTH // 32 key + bodyLength is total body length，还要减去Sandwich头和尾部的字节
            // head is [0, 8) length, [8, 40) key
          _connector.get.write(BackupCommand.headBytes(magic = "SAND", event.key, bodyLength), Some(_connection.get)) onComplete {
            case Success(x) ⇒ logger.debug(s"write to ${_connection.get}, success? $x")
            case Failure(t) ⇒ logger.error(s"write to ${_connection.get} failed", t)
          }
            logger.debug("writed backup head")
            md5.reset()
            val out = new OutputStream {
              val buffer: ListBuffer[Byte] = ListBuffer.empty
              val bufSize: Int             = 1 * 1024 * 1024 * 4
              def write(b: Int): Unit = {
                if (buffer.size == bufSize) {
                  val bytes = buffer.toArray
                  md5.update(bytes)
                  _connector.get.write(ByteString(bytes), Some(_connection.get)) onComplete {
                    case Success(x) ⇒ logger.debug(s"write to ${_connection.get}, success? $x")
                    case Failure(t) ⇒ logger.error(s"write to ${_connection.get} failed", t)
                  }
                  logger.debug(s"writed $bufSize Bytes")
                  buffer.clear()
                }
                buffer.append(b.toByte)
                ()
              }

              override def close(): Unit = {
                if (buffer.nonEmpty) {
                  val bytes = buffer.toArray
                  md5.update(bytes)
                  _connector.get.write(ByteString(bytes), Some(_connection.get)) onComplete {
                    case Success(x) ⇒ logger.debug(s"write to ${_connection.get}, success? $x")
                    case Failure(t) ⇒ logger.error(s"write to ${_connection.get} failed", t)
                  }
                  logger.debug(
                    s"closing, rest buffered writed ${buffer.size} Bytes, md5 is ${md5.digest().map("%02x" format _).mkString("")}")
                  buffer.clear()
                }
                super.close()
              }
            }
            GetCommand._runWithKey(coreConfig, event.key, out) match {
              case Success(_) ⇒ logger.info(s"backup successfylly for ${event.key}")
              case Failure(t) ⇒ logger.error(s"backup failed for ${event.key}", t)
            }
            out.close()
          case None ⇒ Future.failed(new Exception(s"${event.key} not found in ufs3"))
        }
        ()
      }

    }
  })

  def backup(key: String): Unit = {
    queue.put(BackupEvent(key))
  }

  def start(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    targetAddress.foreach(x ⇒ {
      createClient(x).onComplete {
        case Success((connector, Some(connection))) ⇒
          tcpConnector.set(Some(connector))
          tcpConnection.set(Some(connection))
          logger.info(s"backup client connected to $x")
          thread.start()

        case Failure(t) ⇒
          logger.error(s"backup client connect to $x failed", t)

        case _ ⇒ //ignore
      }
    })

  }

  private def createClient(target: InetSocketAddress)(
      implicit system: ActorSystem,
      ec: ExecutionContext): Future[(TcpConnector[Connection], Option[Connection])] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val c = TcpConnector.createClient(Vector(target))
    val s = c.startup()
    c.registerConnectionVisitor(PingVisitor("PING\r\n"))
    c.registerConnectionVisitor(AutoReconnectVisitor(10.seconds))
    c.registerConnectionVisitor(new AbstractConnectionVisitor {
      override def onClosed(connector: TcpConnector[Connection], connection: Connection): Unit = {
        logger.warn(s"connection closed: $connection")
      }
    })
    s
  }
}

object BackupSingleThread {
  def apply(_config: CoreConfig, target: Option[InetSocketAddress]): BackupSingleThread = new BackupSingleThread() {
    val coreConfig: Data.CoreConfig              = _config
    val targetAddress: Option[InetSocketAddress] = target
  }
}
