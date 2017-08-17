package ufs3.integ.http_server.actor

import java.io.{InputStream, PipedInputStream, PipedOutputStream}
import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorContext, ActorRef, ActorSystem, Props}
import akka.stream.scaladsl.Source
import akka.util.{ByteString, Timeout}
import akka.pattern._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.StreamConverters
import ufs3.core.data

import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scala.annotation.tailrec
import UploadLogger.logger
import org.slf4j.{Logger, LoggerFactory}
import ufs3.integ.http_server.backup.BackupSingleThread
import ufs3.kernel.commons.{Config, UFS3}

object UploadLogger {
  val logger: Logger = LoggerFactory.getLogger("upload")
}

trait UploadProxyActor extends Actor {

  import PipeLineStreamActor._
  import PutActor._
  implicit val timeout = Timeout(30.minute)

  implicit val materializer = ActorMaterializer()

  val coreConfig: Config
  val backupTarget: Option[InetSocketAddress]

  def receive: Receive = {
    case UploadProxyActor.Events.UploadRequest(key, source) ⇒
      //      val in  = new PipedInputStream(4 * 1024 * 1024)
      //      val out = new PipedOutputStream()
      // ask Pipeline actor to handle source → out → in
      //      pipelineStreamActor.ask(PipeLineStreamActor.Events.PipeLineStream(in, out, source))
      val in = source.runWith(StreamConverters.asInputStream(readTimeout = Duration(10, TimeUnit.MINUTES)))
      // ask Put actor to put in → ufs3
      val f2 = putActorRef(coreConfig, backupTarget).ask(PutActor.Events.Put(key, in))
      import context.dispatcher

      val _sender = sender()

      f2.onComplete {
        case Success(x) ⇒
          _sender ! x
          in.close()
        //          out.close()

        case Failure(t) ⇒
          _sender ! Some(t)
          in.close()
        //          out.close()
      }

  }
}
object UploadProxyActor {
  object Events {
    final case class UploadRequest(key: String, source: Source[ByteString, Any])
  }

  def props(_coreConfig: Config, _backupTarget: Option[InetSocketAddress]): Props = {
    Props(new UploadProxyActor {
      val coreConfig: Config                  = _coreConfig
      val backupTarget: Option[InetSocketAddress] = _backupTarget
    })
  }

  //smart constructor
  def uploadProxyActorRef(coreConfig: Config, backupTarget: Option[InetSocketAddress])(
    implicit system: ActorSystem): ActorRef = system.actorOf(props(coreConfig, backupTarget))
}

trait PipeLineStreamActor extends Actor {
  import PipeLineStreamActor._

  implicit val materializer = ActorMaterializer()
  def receive: Receive = {
    case Events.PipeLineStream(in, out, source) ⇒
      out.connect(in)
      val sink = StreamConverters.fromOutputStream(() ⇒ out)
      import context.dispatcher
      val _sender = sender()
      source
        .runWith(sink)
        .map(ioResult ⇒ {
          _sender ! 0
          context.stop(self)
        })
        .recover {
          case t: Throwable ⇒
            _sender ! 0
            context stop self
        }
      ()

  }
}

object PipeLineStreamActor {
  object Events {
    final case class PipeLineStream(in: PipedInputStream, out: PipedOutputStream, source: Source[ByteString, Any])
  }

  def props(): Props = {
    Props(new PipeLineStreamActor {})
  }

  def pipelineStreamActor(implicit ac: ActorContext): ActorRef = ac.actorOf(props())
}

trait PutActor extends Actor {
  import PutActor._
  val coreConfig: Config
  val backupTarget: Option[InetSocketAddress]

  lazy val ufs3: UFS3 = {
    if (ufs3Holder.get().isDefined) ufs3Holder.get().get
    else {
      PutCommand.writableUfs3(coreConfig) match {
        case Left(t) ⇒ throw t
        case Right(b) ⇒
          ufs3Holder.set(Some(b))
          b
      }
    }
  }
  lazy val backupThread: AtomicReference[Option[BackupSingleThread]] = {
    if (backupThreadHolder.get().isDefined) backupThreadHolder
    else {
      backupThreadHolder.set({
        backupTarget.map(_ ⇒ {
          val backup = BackupSingleThread(coreConfig, backupTarget)
          backup.start()
          backup
        })
      })
      backupThreadHolder
    }

  }

  def receive: Receive = {
    case Events.Put(key, ins) ⇒
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
            PutCommand._runWithUfs3(coreConfig, key, ins, ufs3) match {
              case Right(_) ⇒ // println("put ok")
                _sender ! None
              case Left(x) ⇒ //x.printStackTrace()
                _sender ! Some(x)
            }
        }
      }
      write()

      // start backup
      // use a fixed thread pool
      backupThread.get().foreach(_.backup(key))
      context.stop(self)
  }
}

object PutActor {
  object Events {
    final case class Put(key: String, ins: InputStream)
  }

  val backupThreadHolder: AtomicReference[Option[BackupSingleThread]] =
    new AtomicReference[Option[BackupSingleThread]](None)
  val ufs3Holder: AtomicReference[Option[UFS3]] = new AtomicReference[Option[data.Data.UFS3]](None)

  def props(_coreConfig: Config, _backupTarget: Option[InetSocketAddress]): Props =
    Props(new PutActor {
      val coreConfig: Config                  = _coreConfig
      val backupTarget: Option[InetSocketAddress] = _backupTarget
    })
  def putActorRef(coreConfig: Config, _backupTarget: Option[InetSocketAddress])(
    implicit ac: ActorContext): ActorRef = ac.actorOf(props(coreConfig, _backupTarget))
}
