/**
  * command_backup.scala
  * ----------
  * backup command implementation
  * @author bigknife
  * @since 2017/7/26
  */
package ufs3
package integration
package command

import java.io.OutputStream
import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.util.ByteString
import com.barcsys.tcp.connection.buffer.{BufferSettings, IOBufferSettings}
import com.barcsys.tcp.connection.visitor.{AbstractConnectionVisitor, PingVisitor}
import com.barcsys.tcp.connection.{Connection, TcpConnector}
import com.typesafe.config.{Config, ConfigFactory}
import pharaoh.LogbackConfExtension
import ufs3.core.data.Data._
import ufs3.interpreter.layout.{SandwichHeadLayout, SandwichTailLayout}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._


trait BackupCommand {

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

  // backup the file identified by the key to target
  def backup(coreConfig: CoreConfig, key: String, target: InetSocketAddress): Future[Boolean] = {
    val promise = Promise[Boolean]
    val s = startBackupServerClient(target, promise).flatMap {
      case (connector, _) ⇒
        // query idx of the key, write tcp head
        val f = Future.successful {
          GetCommand._idxOfKey(coreConfig, key).map {
            case Some(idx) ⇒
              // 1. Magic: "SAND"
              // 2. BODY_LENGTH, 8 (3#32 + 4#bodylength)
              // 3. KEY, 32
              // 4. body stream
              val bodyLength = 32 + idx.endPoint - idx.startPoint - SandwichHeadLayout.HEAD_LENGTH - SandwichTailLayout.TAIL_LENGTH // 32 key + bodyLength is total body length，还要减去Sandwich头和尾部的字节
              // head is [0, 8) length, [8, 40) key
              connector.write(headBytes(magic = "SAND", key, bodyLength))
              val out = new OutputStream {
                val buffer: ListBuffer[Byte] = ListBuffer.empty
                val bufSize: Int = 1 * 1024 * 1024 * 4
                def write(b: Int): Unit = {
                  if(buffer.size == bufSize) {
                    connector.write(ByteString(buffer.toArray))
                    buffer.clear()
                  }else {
                    buffer.append(b.toByte)
                  }
                  ()
                }

                override def close(): Unit = {
                  if(buffer.nonEmpty) {
                    connector.write(ByteString(buffer.toArray))
                    buffer.clear()
                  }
                  super.close()
                }
              }
              GetCommand._runWithKey(coreConfig, key, out)
              out.close()
            case None ⇒ Future.failed(new Exception(s"$key not found in ufs3"))
          }
          ()
        }
        f.flatMap(_ ⇒ promise.future)
    }

    for {
      b ← s
      _ ← system.terminate()
    } yield b
  }

  def headBytes(magic: String, key: String, bodyLength: Long): ByteString = {
    import ufs3.interpreter.layout.Layout._
    ByteString(magic) ++ ByteString(bodyLength.`8Bytes`.bytes) ++ ByteString(key)
  }

  def startBackupServerClient(target: InetSocketAddress, promise: Promise[Boolean]): Future[(TcpConnector[Connection], Option[Connection])] = {
    val returnVisitor = new AbstractConnectionVisitor(){
      override def onRead(connector: TcpConnector[Connection], connection: Connection, data: ByteString): ByteString = {
        if (data.size == 1) {
          val code = data.mkString("")
          promise.success(code == "1")
          ByteString.empty
        }
        else data
      }
    }
    val c = TcpConnector.createClient(Vector(target))
    val s = c.startup()
    c.registerConnectionVisitor(PingVisitor("PING"))
    c.registerConnectionVisitor(returnVisitor)
    s
  }

}

object BackupCommand extends BackupCommand
