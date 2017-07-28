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
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.ByteString
import com.barcsys.tcp.connection.{Connection, TcpConnector}
import com.barcsys.tcp.connection.visitor.AbstractConnectionVisitor
import org.apache.log4j.Logger
import ufs3.core.data.Data._
import ufs3.integration.command.backupserver.BackupActor.{BackupNewFile, BackupWriteData, ReadCompleted, WriteCompleted}
import ufs3.integration.command.backupserver.PutActor.RunWithUFS3

import scala.util.{Failure, Success}

object BackupLogger {
  val logger: Logger = Logger.getLogger("backup")
}

class PutActor(coreConfig: CoreConfig) extends Actor {
  import BackupLogger._
  def receive: Receive = {
    case RunWithUFS3(key, in, ufs3) ⇒
      val _sender = sender()
      PutCommand._runWithUfs3(coreConfig, key, in, ufs3) match {
        case Success(str) ⇒
          logger.debug(s"put command successfully $str")
          _sender ! WriteCompleted(key, None)

        case Failure(t) ⇒
          //logger.error("put command failed", t)
          _sender ! WriteCompleted(key, Some(t))
      }
      in.close()
      context stop self
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

  private def init(key: String, connector: TcpConnector[Connection], connection: Connection): Unit = {
    val in  = new PipedInputStream()
    val out = new PipedOutputStream()
    out.connect(in)
    ioMap.set(Map(key      → (in, out)))
    writedMap.set(Map(key  → 0))
    connectMap.set(Map(key → (connector, connection)))
  }
  private def destroy(): Unit = {
    ioMap.set(Map.empty)
    writedMap.set(Map.empty)
    connectMap.set(Map.empty)
  }

  def receive: Receive = {
    case BackupNewFile(key, totalLength, connector, connection) ⇒
      logger.debug(s"backup new file：$key, total: $totalLength")
      init(key, connector, connection)
      val (in, _)  = ioMap.get()(key)
      val actorRef = context.actorOf(PutActor.props(coreConfig))
      actorRef ! RunWithUFS3(key, in, ufs3.get())

    case BackupWriteData(key, data) ⇒
      if (ioMap.get().contains(key)) {
        val (_, out) = ioMap.get()(key)
        if (out != null) {
          logger.debug(s"writing $key, ${data.size} Bytes")
          out.write(data.toArray[Byte])
          logger.debug(s"writed $key, ${data.size} Bytes")
        }
      }

    case ReadCompleted(key) ⇒
      if (ioMap.get().contains(key)) {
        val (_, out) = ioMap.get()(key)
        logger.info(s"read from client completed：$key")
        out.close()
      }

    case WriteCompleted(key, ex) ⇒
      logger.info(s"writing complete, $key, ${if (ex.isEmpty) "successfully" else ex.get.getMessage}")
      val (in, _) = ioMap.get()(key)
      in.close()
      // 告知客户端写入完成
      val (connector, connection) = connectMap.get()(key)
      // Byte 1 → success, // Byte 0 → failed
      val code = if (ex.isEmpty) 1.toByte else 0.toByte
      import scala.concurrent.ExecutionContext.Implicits.global
      connector.write(ByteString(code), Some(connection)).onComplete {
        case Success(true) ⇒ logger.info(s"write back to $connection successfully"); connection.close()
        case Success(false) ⇒ logger.info(s"write back to $connection failed"); connection.close()
        case Failure(t) ⇒ logger.error(s"write back to $connection exception", t); connection.close()
      }
      destroy()

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
    require(magic sameElements "SAND".getBytes, "backup messages magic should be SAND")
    val bodyLength = bytes.slice(4, 12).`8Bytes`.longValue
    val key        = new String(bytes.slice(12, 44), "UTF-8")
    Pointer(key, 0, bodyLength - 32) // key的32字节此时已经读取，所以要减去
  }
}

class BackupVisitor(coreConfig: CoreConfig, actorRef: ActorRef) extends AbstractConnectionVisitor {
  import Pointer._

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

  override def onRead(connector: TcpConnector[Connection], connection: Connection, data: ByteString): ByteString = {
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
        }
        // 2. 读取body
        if (afterFillHeadBuffer.nonEmpty) {
          // 如果超出totalsize，则递归consume
          val pointer = currentKey.get().get
          val body    = afterFillHeadBuffer.slice(0, pointer.remained.toInt)
          actorRef ! BackupActor.BackupWriteData(pointer.key, body)
          //更新currentKey
          val newPointer = pointer.copy(writed = pointer.writed + body.size)
          currentKey.set(Some(newPointer))
          // 如果afterFill。。。还有剩下的，则递归
          val rest = afterFillHeadBuffer.drop(pointer.total.toInt)
          // 3. 清空headBuffer
          if (newPointer.completed) {
            headBuffer.set(ByteString.empty)
            actorRef ! BackupActor.ReadCompleted(newPointer.key)
          }
          if (rest.nonEmpty) consume(rest) else ByteString.empty
        } else ByteString.empty
      }
    }

    consume(data)
  }
}
