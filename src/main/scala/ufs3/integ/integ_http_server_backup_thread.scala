package ufs3.integ.http_server.backup


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
import org.slf4j.{Logger, LoggerFactory}
import pharaoh.LogbackConfExtension
import ufs3.integ.httpServer
import ufs3.interp.commons.Stack
import ufs3.kernel.commons.{Idx, SandwichHeadLayout, SandwichTailLayout, Config => UFS3Config}
import ufs3.kernel.exceptions.KeyNotFoundException
import ufs3.kernel.modules.App

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import ufs3.prog._

object BackupThreadLogger {
  val logger: Logger = LoggerFactory.getLogger("backup-thread")
}

trait BackupSingleThread {
  import BackupThreadLogger._

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

  val coreConfig: UFS3Config
  val targetAddress: Option[InetSocketAddress]

  val tcpConnector: AtomicReference[Option[TcpConnector[Connection]]] =
    new AtomicReference[Option[TcpConnector[Connection]]](None)
  val tcpConnection: AtomicReference[Option[Connection]] = new AtomicReference[Option[Connection]](None)

  case class BackupEvent(key: String)

  // a bounded linked list blocking queue
  // max is Integer.MAX
  private val queue = new LinkedBlockingQueue[BackupEvent]()

  val lock = new Object

  lazy val thread: Thread = {
    val t = new Thread(new Runnable {
      def run(): Unit = {
        handleQueue(queue)
      }
      @tailrec
      private def handleQueue(queue: LinkedBlockingQueue[BackupEvent]): Unit = {
        logger.info("handling backup thread queue")
        val event = queue.take()
        logger.info("got event from backup thread queu")
        workFor(event) match {
          case Right(_) ⇒
            // 只有向备份服务器发出备份操作，才能加锁
            logger.info(s"handled event $event, lock backup thread")
            lock.synchronized { lock.wait() }
          case Left(t1) ⇒
            logger.error(s"handled event $event, but some exception raised", t1)
        }

        handleQueue(queue)
      }
      def headBytes(magic: String, key: String, bodyLength: Long): ByteString = {
        import ufs3.kernel.commons.Layout._
        ByteString(magic) ++ ByteString(bodyLength.`8Bytes`.bytes) ++ ByteString(key)
      }
      private def workFor(event: BackupEvent): Either[Throwable, Unit] = {
        logger.debug(s"Got backup event: $event")

        val _connector  = tcpConnector.get()
        val _connection = tcpConnection.get()
        if (_connector.isDefined) {
          val md5 = MessageDigest.getInstance("md5")
          Try {
            Stack.parseApp[Option[Idx]](fetchKey[App.Op](event.key, httpServer.openReadonlyUFS3(coreConfig))).run(coreConfig).unsafeRun()
          } match {
            case Success(Some(idx)) ⇒
              // 1. Magic: "SAND"
              // 2. BODY_LENGTH, 8 (3#32 + 4#bodylength)
              // 3. KEY, 32
              // 4. body stream
              val bodyLength = 32 + idx.endPoint - idx.startPoint - SandwichHeadLayout.HEAD_LENGTH - SandwichTailLayout.TAIL_LENGTH // 32 key + bodyLength is total body length，还要减去Sandwich头和尾部的字节
              // head is [0, 8) length, [8, 40) key
              writeUntilSuccess(_connector.get,
                _connection,
                headBytes(magic = "SAND", event.key, bodyLength))
              logger.debug(s"writed backup head for key: ${event.key}")
              md5.reset()
              val out = new OutputStream {
                val buffer: ListBuffer[Byte] = ListBuffer.empty
                val bufSize: Int             = 1 * 1024 * 1024 * 4
                def write(b: Int): Unit = {
                  if (buffer.size == bufSize) {
                    val bytes = buffer.toArray
                    md5.update(bytes)
                    writeUntilSuccess(_connector.get, _connection, ByteString(bytes))
                    logger.debug(s"writed $bufSize Bytes for key: ${event.key}")
                    buffer.clear()
                  }
                  buffer.append(b.toByte)
                  ()
                }

                override def close(): Unit = {
                  if (buffer.nonEmpty) {
                    val bytes = buffer.toArray
                    md5.update(bytes)
                    writeUntilSuccess(_connector.get, _connection, ByteString(bytes))
                    logger.debug(
                      s"closing for key: ${event.key}, rest buffered writed ${buffer.size} Bytes, md5 is ${md5.digest().map("%02x" format _).mkString("")}")
                    buffer.clear()
                  }
                  super.close()
                }
              }
              Try {
                Stack.parseApp(read[App.Op](event.key, httpServer.openReadonlyUFS3(coreConfig), out)).run(coreConfig).unsafeRun()
              } match {
                case Success(_) ⇒
                  logger.info(s"backup successfully for ${event.key}")
                  out.close()
                  Right(())

                case Failure(t1) ⇒
                  logger.error(s"backup failed for ${event.key}", t1)
                  out.close()
                  Left(t1)
              }

            case Success(None) ⇒
              logger.error(s"${event.key} not found in ufs3")
              Left(KeyNotFoundException(key = event.key))

            case Failure(t1) ⇒
              logger.error(s"backup failed for ${event.key}", t1)
              Left(t1)
          }
        }
        else Left(new IllegalStateException("connection to back up server not connected now"))
      }
    })
    t.setName("backup-thread")
    t
  }

  //todo 需要优化
  private def writeUntilSuccess(connector: TcpConnector[Connection],
                                connection: Option[Connection],
                                bs: ByteString): Unit = {
    val f = connector.write(bs, connection)
    try {
      val res = Await.result(f, Duration.Inf)
      if (!res) {
        logger.warn(s"write to $connection return false, will retry after 1.sec")
        Thread.sleep(1000)
        writeUntilSuccess(connector, connection, bs)
      }
    } catch {
      case t: java.lang.RuntimeException if t.getMessage contains "output buffer is overflow" ⇒
        logger.warn(s"write to $connection failed, will retry after 1.sec")
        Thread.sleep(1000)
        writeUntilSuccess(connector, connection, bs)
    }
  }

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
    c.registerConnectionVisitor(new AbstractConnectionVisitor {
      override def onConnected(connector: TcpConnector[Connection], connection: Connection): Unit = {
        logger.info(s"connected to $connection")
        tcpConnection.set(Some(connection))
        tcpConnector.set(Some(connector))
      }
    })
    val returnVisitor = new AbstractConnectionVisitor() {
      override def onRead(connector: TcpConnector[Connection], connection: Connection, data: ByteString): ByteString = {
        if (data.size == 1) {
          val code = data.mkString("")
          //promise.success(code == "1")
          // backup server 回复备份成功，才进行下一个备份
          if (code == "1") {
            lock.synchronized {
              logger.info(s"Backup server has given successful back message, notify go on next backup event")
              lock.notifyAll()
            }
          }
          ByteString.empty
        } else data
      }
    }
    c.registerConnectionVisitor(returnVisitor)
    s
  }
}

object BackupSingleThread {
  def apply(_config: UFS3Config, target: Option[InetSocketAddress]): BackupSingleThread = new BackupSingleThread() {
    val coreConfig: UFS3Config              = _config
    val targetAddress: Option[InetSocketAddress] = target
  }
}
