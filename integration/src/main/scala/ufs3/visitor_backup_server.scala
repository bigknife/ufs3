/**
  * visitor_backup_server.scala
  * ----------
  * backup server visitor
  * @author bigknife
  * @since 2017/7/27
  */
package ufs3
package integration
package command
package backupserver

import java.io.{InputStream, PipedInputStream, PipedOutputStream}
import java.security.MessageDigest
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.ByteString
import com.barcsys.tcp.connection.{Connection, TcpConnector}
import com.barcsys.tcp.connection.visitor.AbstractConnectionVisitor
import org.apache.log4j.Logger
import ufs3.core.data.Data._
import ufs3.integration.command.backupserver.BackupActor.{BackupNewFile, BackupWriteData, ReadCompleted, WriteCompleted}
import ufs3.integration.command.backupserver.PutActor.RunWithUFS3

import scala.annotation.tailrec
import scala.util.{Failure, Success}

object BackupLogger {
  val logger: Logger = Logger.getLogger("backup")
}

class PutActor(coreConfig: CoreConfig) extends Actor {
  import BackupLogger._
  def receive: Receive = {
    case RunWithUFS3(key, in, ufs3) ⇒
      val _sender = sender()
      // 首先需要检查是否是正在写入，如果是，则要自旋等待写入完成
      @tailrec
      def write(): Unit = {
        PutCommand._ufs3IsWriting(coreConfig, ufs3) match {
          case Left(error) ⇒
            logger.warn(s"$error, wating 1.sec and retry to write $key")
            Thread.sleep(1000)
            write()
          case Right(_) =>
            PutCommand._runWithUfs3(coreConfig, key, in, ufs3) match {
              case Success(str) ⇒
                logger.debug(s"put command successfully $str")
                _sender ! WriteCompleted(key, None)

              case Failure(t) ⇒
                logger.error("put command failed", t)
                _sender ! WriteCompleted(key, Some(t))
            }
        }
      }
      write()
  }
}

object PutActor {
  final case class RunWithUFS3(key: String, in: InputStream, ufs3: UFS3)
  def props(coreConfig: CoreConfig): Props = Props(new PutActor(coreConfig))
}

class BackupActor(coreConfig: CoreConfig) extends Actor {
  import BackupLogger._

  val ufs3 = new AtomicReference[UFS3]()
  override def preStart(): Unit = {
    ufs3.set(PutCommand.writableUfs3(coreConfig))
    super.preStart()
  }

  val ioMap: AtomicReference[Map[String, (PipedInputStream, PipedOutputStream)]] =
    new AtomicReference[Map[String, (PipedInputStream, PipedOutputStream)]](Map.empty)
  val writedMap: AtomicReference[Map[String, Long]] = new AtomicReference[Map[String, Long]](Map.empty)
  val connectMap: AtomicReference[Map[String, (TcpConnector[Connection], Connection)]] =
    new AtomicReference[Map[String, (TcpConnector[Connection], Connection)]](Map.empty)

  val md5Map: AtomicReference[Map[String, MessageDigest]] = new AtomicReference[Map[String, MessageDigest]](Map.empty)

  private def init(key: String, connector: TcpConnector[Connection], connection: Connection): Unit = {
    val in  = new PipedInputStream(4 * 1024 * 1024)
    val out = new PipedOutputStream()
    out.connect(in)
    ioMap.set(ioMap.get() + (key           → (in, out)))
    writedMap.set(writedMap.get() + (key   → 0))
    connectMap.set(connectMap.get() + (key → (connector, connection)))
    md5Map.set(md5Map.get() + (key         → MessageDigest.getInstance("md5")))
  }
  private def destroy(key: String): Unit = {
    ioMap.set(ioMap.get() - key)
    writedMap.set(writedMap.get() - key)
    connectMap.set(connectMap.get() - key)
    val md5 = md5Map.get()(key)
    logger.info(s"md5 for key=$key is ${md5.digest().map("%02x" format _).mkString("")}")
    md5Map.set(md5Map.get() - key)
  }
  private lazy val putActor: ActorRef = {
    context.actorOf(PutActor.props(coreConfig))
  }


  def receive: Receive = {
    case BackupNewFile(key, totalLength, connector, connection) ⇒
      logger.debug(s"backup new file：$key, total: $totalLength")
      init(key, connector, connection)
      val (in, _)  = ioMap.get()(key)
      putActor ! RunWithUFS3(key, in, ufs3.get())

    case BackupWriteData(key, data) ⇒
      logger.debug(s"Got BackupWriteData message for key=$key")

      val (_, out) = ioMap.get()(key)
      if (out != null) {
        logger.debug(s"writing $key, ${data.size} Bytes")
        val bytes = data.toArray
        val md5   = md5Map.get()(key)
        md5.update(bytes)
        out.write(bytes)
        //out.flush()
        logger.debug(s"writed $key, ${data.size} Bytes")
      }

    case ReadCompleted(key) ⇒
      val (_, out) = ioMap.get()(key)
      logger.info(s"read from client completed：$key")
      out.close()

    case WriteCompleted(key, ex) ⇒
      if (ex.nonEmpty) logger.error("Write completed failed", ex.get)
      logger.info(s"writing complete, $key, ${if (ex.isEmpty) "successfully" else ex.get.getMessage}")
      val (in, _) = ioMap.get()(key)
      in.close()
      // 告知客户端写入完成
      val (connector, connection) = connectMap.get()(key)
      // Byte 1 → success, // Byte 0 → failed
      val code = if (ex.isEmpty) 1.toByte else 0.toByte
      import scala.concurrent.ExecutionContext.Implicits.global
      connector.write(ByteString(code), Some(connection)).onComplete {
        case Success(true)  ⇒ logger.info(s"write back to $connection successfully")
        case Success(false) ⇒ logger.info(s"write back to $connection failed")
        case Failure(t)     ⇒ logger.error(s"write back to $connection exception", t)
      }
      destroy(key)

  }
}

object BackupActor {
  final case class BackupNewFile(key: String,
                                 totalLength: Long,
                                 connector: TcpConnector[Connection],
                                 connection: Connection)
  final case class BackupWriteData(key: String, data: ByteString)
  final case class ReadCompleted(key: String)
  final case class WriteCompleted(key: String, ex: Option[Throwable])

  def props(coreConfig: CoreConfig): Props = Props(new BackupActor(coreConfig))

  def backupActorRef(coreConfig: CoreConfig)(implicit system: ActorSystem): ActorRef = system.actorOf(props(coreConfig))
}

case class Pointer(key: String, writed: Long, total: Long) {
  def completed: Boolean = total == writed
  def remained: Long     = total - writed
}
object Pointer {
  val HeadWithKeySize: Int = 44

  import ufs3.interpreter.layout.Layout._
  def resolve(bytes: Bytes): Pointer = {
    require(bytes.length == HeadWithKeySize, s"backup messages pointer size should be $HeadWithKeySize")
    val magic = bytes.take(4)
    require(magic sameElements "SAND".getBytes, s"backup messages magic should be SAND, but ${new String(magic)}")
    val bodyLength = bytes.slice(4, 12).`8Bytes`.longValue
    val key        = new String(bytes.slice(12, 44), "UTF-8")
    Pointer(key, 0, bodyLength - 32) // key的32字节此时已经读取，所以要减去
  }
}

class BackupVisitor(coreConfig: CoreConfig, actorRef: ActorRef) extends AbstractConnectionVisitor {
  import Pointer._
  import BackupLogger._

  val currentKey: AtomicReference[Option[Pointer]] = new AtomicReference[Option[Pointer]](None)
  val headBuffer: AtomicReference[ByteString]      = new AtomicReference[ByteString](ByteString.empty)

  def init(): Unit = {
    currentKey.set(None)
    headBuffer.set(ByteString.empty)

  }

  def isANewFile: Boolean       = currentKey.get().isEmpty || currentKey.get().get.completed
  def isHeadBufferFull: Boolean = headBuffer.get().size == HeadWithKeySize
  // 塞满headBuffer，返回剩余的
  def fillHeadBuffer(bs: ByteString): ByteString = {
    val cnt = HeadWithKeySize - headBuffer.get().size
    if (cnt == 0) bs
    else {
      val _bs = bs.take(cnt)
      headBuffer.set(headBuffer.get() ++ _bs)
      bs.drop(cnt)
    }
  }
  // 同时连接只能有一个
  val connectionCount = new AtomicInteger(0)

  override def onClosed(connector: TcpConnector[Connection], connection: Connection): Unit = {
    connectionCount.decrementAndGet()
    ()
  }

  override def onConnected(connector: TcpConnector[Connection], connection: Connection): Unit = {
    if (connectionCount.incrementAndGet() > 1) {
      connection.close()
      ()
    } else {
      // 初始化
      init()
    }
  }

  //val readed = new AtomicLong(0)

  override def onRead(connector: TcpConnector[Connection], connection: Connection, data: ByteString): ByteString = {
    //val l = readed.addAndGet(data.size.toLong)
    //println(s"本地读取 ${data.size}, 累计: $l")
    // return rest
    def consume(bs: ByteString): ByteString = {
      if (bs.isEmpty) bs
      else {
        // 1. 填充headBuffer
        val afterFillHeadBuffer = fillHeadBuffer(bs)
        if (isHeadBufferFull && isANewFile) {
          // 如果填充完整，则发出准备写入事件
          val pointer = Pointer.resolve(headBuffer.get().toArray[Byte])
          currentKey.set(Some(pointer))
          actorRef ! BackupActor.BackupNewFile(pointer.key, pointer.total, connector, connection)
          logger.info(s"Sent BackupNewFile message: $pointer")
        }
        // 2. 读取body
        if (afterFillHeadBuffer.nonEmpty) {
          // 如果超出totalsize，则递归consume
          val pointer = currentKey.get().get
          val remained = pointer.remained.toInt
          val body    = afterFillHeadBuffer.slice(0, remained)
          actorRef ! BackupActor.BackupWriteData(pointer.key, body)
          logger.debug(s"Sent BackupWriteData message: $pointer")
          //更新currentKey
          val newPointer = pointer.copy(writed = pointer.writed + body.size)
          currentKey.set(Some(newPointer))
          // 如果afterFill。。。还有剩下的，则递归
          val rest = afterFillHeadBuffer.drop(remained)
          // 3. 清空headBuffer
          if (newPointer.completed) {
            headBuffer.set(ByteString.empty)
            actorRef ! BackupActor.ReadCompleted(newPointer.key)
            logger.debug(s"Sent ReadCompleted message: $pointer")
          }
          if (rest.nonEmpty) consume(rest) else ByteString.empty
        } else ByteString.empty
      }
    }

    consume(data)
    //ByteString.empty
  }
}
