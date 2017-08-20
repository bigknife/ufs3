package ufs3.integ

import java.io.OutputStream
import java.net.InetSocketAddress
import java.security.MessageDigest

import akka.actor.ActorSystem
import akka.util.ByteString
import com.barcsys.tcp.connection.buffer.{BufferSettings, IOBufferSettings}
import com.barcsys.tcp.connection.{Connection, TcpConnector}
import com.barcsys.tcp.connection.visitor.{AbstractConnectionVisitor, PingVisitor}
import com.typesafe.config.{Config, ConfigFactory}
import fs2.Task
import org.slf4j.{Logger, LoggerFactory}
import pharaoh.LogbackConfExtension
import ufs3.interp.commons.Stack
import ufs3.kernel.commons.{Idx, SandwichHeadLayout, SandwichTailLayout}
import ufs3.kernel.modules.App
import ufs3.world.commons.BackupArg

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import ufs3.prog._

import scala.util.{Failure, Success, Try}

object backupClient {
  def apply(arg: BackupArg): Task[Unit] = Task.delay {
    import scala.concurrent.ExecutionContext.Implicits.global
    val key = arg.key.get
    val promise = Promise[Boolean]
    val targetAddress = arg.target.get.split(":")
    val target = new InetSocketAddress(targetAddress(0), targetAddress(1).toInt)
    val future = for {
      (connector, _) ← startBackupServerClient(target, promise)
    } yield {
      val fetchP =fetchKey[App.Op](arg.key.get, httpServer.openReadonlyUFS3(arg.asConfig))
      Stack.parseApp[Option[Idx]](fetchP).run(arg.asConfig).unsafeRun() match {
        case Some(idx) ⇒
          // 1. Magic: "SAND"
          // 2. BODY_LENGTH, 8 (3#32 + 4#bodylength)
          // 3. KEY, 32
          // 4. body stream
          val bodyLength = 32 + idx.endPoint - idx.startPoint - SandwichHeadLayout.HEAD_LENGTH - SandwichTailLayout.TAIL_LENGTH // 32 key + bodyLength is total body length，还要减去Sandwich头和尾部的字节
          // head is [0, 8) length, [8, 40) key
          connector.write(headBytes(magic = "SAND", key, bodyLength))
          logger.debug("writed backup head")
          md5.reset()
          val out = new OutputStream {
            val buffer: ListBuffer[Byte] = ListBuffer.empty
            val bufSize: Int = 1 * 1024 * 1024 * 4
            def write(b: Int): Unit = {
              if(buffer.size == bufSize) {
                val bytes = buffer.toArray
                md5.update(bytes)
                connector.write(ByteString(bytes))
                logger.debug(s"writed $bufSize Bytes")
                buffer.clear()
              }
              buffer.append(b.toByte)
              ()
            }

            override def close(): Unit = {
              if(buffer.nonEmpty) {
                val bytes = buffer.toArray
                md5.update(bytes)
                connector.write(ByteString(bytes))
                logger.debug(s"closing, rest buffered writed ${buffer.size} Bytes, md5 is ${md5.digest().map("%02x" format _).mkString("")}")
                buffer.clear()
              }
              super.close()
            }
          }
          Try {
            val p = read[App.Op](key, httpServer.openReadonlyUFS3(arg.asConfig), out)
            Stack.parseApp(p).run(arg.asConfig).unsafeRun()
          } match {
            case Success(_) ⇒
              logger.info("readed from ufs3")
              out.close()
            case Failure(t) ⇒
              logger.error("read from ufs3 error", t)
              out.close()
          }
        case None ⇒
          logger.error(s"no key=$key found in ufs3")
      }
    }

    val finalFuture = for {
      _ ← future
      _ ← promise.future
      _ ← system.terminate()
    } yield ()

    Await.result(finalFuture, Duration.Inf)
  }

  val logger: Logger   = LoggerFactory.getLogger("backup")
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
  val md5: MessageDigest = MessageDigest.getInstance("md5")

  def startBackupServerClient(target: InetSocketAddress,
                              promise: Promise[Boolean]): Future[(TcpConnector[Connection], Option[Connection])] = {
    val returnVisitor = new AbstractConnectionVisitor() {
      override def onRead(connector: TcpConnector[Connection], connection: Connection, data: ByteString): ByteString = {
        if (data.size == 1) {
          val code = data.mkString("")
          promise.success(code == "1")
          ByteString.empty
        } else data
      }
    }
    val c = TcpConnector.createClient(Vector(target))
    val s = c.startup()
    c.registerConnectionVisitor(PingVisitor("PING"))
    c.registerConnectionVisitor(returnVisitor)
    s
  }

  def headBytes(magic: String, key: String, bodyLength: Long): ByteString = {
    import ufs3.kernel.commons.Layout._
    ByteString(magic) ++ ByteString(bodyLength.`8Bytes`.bytes) ++ ByteString(key)
  }
}
