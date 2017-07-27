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

import java.io.{PipedInputStream, PipedOutputStream}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.ByteString
import com.barcsys.tcp.connection.{Connection, TcpConnector}
import com.barcsys.tcp.connection.visitor.AbstractConnectionVisitor
import ufs3.core.data.Data._
import ufs3.integration.command.backupserver.BackupActor.{BackupCompleted, BackupNewFile, BackupWriteData}

import scala.util.{Failure, Success}

class BackupActor(coreConfig: CoreConfig) extends Actor {
  val ufs3 = new AtomicReference[UFS3]()
  override def preStart(): Unit = {
    ufs3.set(PutCommand.writableUfs3(coreConfig))
    super.preStart()
  }

  val ioMap: AtomicReference[Map[String, (PipedInputStream, PipedOutputStream)]] = new AtomicReference[Map[String, (PipedInputStream, PipedOutputStream)]](Map.empty)


  def receive: Receive = {
    case BackupNewFile(key, totalLength) ⇒
      println(s"准备备份新文件：$key, total: $totalLength")
      val in = new PipedInputStream()
      val out = new PipedOutputStream()
      out.connect(in)
      ioMap.set(Map(key → (in, out)))
      // handle try
      PutCommand._runWithUfs3(coreConfig, key, in, ufs3.get()) match {
        case Success(_) ⇒ println("put command successfully")
        case Failure(t) ⇒ t.printStackTrace()
      }
      ()

    case BackupWriteData(key, data) ⇒
      val (_, out) = ioMap.get()(key)
      out.write(data.toArray[Byte])
      println(s"写入文件 $key, ${data.size} 字节")

    case BackupCompleted(key) ⇒
      println(s"写入完成：$key")
      val (in, out) = ioMap.get()(key)
      in.close()
      out.close()
  }
}

object BackupActor {
  final case class BackupNewFile(key: String, totalLength: Long)
  final case class BackupWriteData(key: String, data: ByteString)
  final case class BackupCompleted(key: String)

  def props(coreConfig: CoreConfig): Props = Props(new BackupActor(coreConfig))

  def backupActorRef(coreConfig: CoreConfig)(implicit system: ActorSystem): ActorRef = system.actorOf(props(coreConfig))
}


case class Pointer(key: String, writed: Long, total: Long) {
  def completed: Boolean = total == writed
  def remained: Long = total - writed
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
  val headBuffer: AtomicReference[ByteString] = new AtomicReference[ByteString](ByteString.empty)

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
    }else {
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
          actorRef ! BackupActor.BackupNewFile(pointer.key, pointer.total)
        }
        // 2. 读取body
        if (afterFillHeadBuffer.nonEmpty) {
          // 如果超出totalsize，则递归consume
          val pointer = currentKey.get().get
          val body = afterFillHeadBuffer.slice(0, pointer.remained.toInt)
          actorRef ! BackupActor.BackupWriteData(pointer.key, body)
          //更新currentKey
          val newPointer = pointer.copy(writed = pointer.writed + body.size)
          currentKey.set(Some(newPointer))
          // 如果afterFill。。。还有剩下的，则递归
          val rest = afterFillHeadBuffer.drop(pointer.total.toInt)
          // 3. 清空headBuffer
          if(newPointer.completed) {
            println("clear----------")
            headBuffer.set(ByteString.empty)
            actorRef ! BackupActor.BackupCompleted(newPointer.key)
          }
          if (rest.nonEmpty) consume(rest) else ByteString.empty
        } else ByteString.empty
      }
    }

    consume(data)
  }
}
