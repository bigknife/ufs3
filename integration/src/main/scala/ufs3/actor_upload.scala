/**
  * actor_upload.scala
  * ----------
  * actor class of upload
  * @author bigknife
  * @since 2017/7/25
  */
package ufs3
package integration
package command
package actor

import java.io.{InputStream, PipedInputStream, PipedOutputStream}
import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorContext, ActorRef, ActorSystem, Props}
import akka.stream.scaladsl.{Sink, Source}
import akka.util.{ByteString, Timeout}
import akka.pattern._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.StreamConverters
import ufs3.core.data.Data

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import ufs3.core.data.Data._


trait UploadProxyActor extends Actor {

  import PipeLineStreamActor._
  import PutActor._
  implicit val timeout = Timeout(30.minute)

  val coreConfig: CoreConfig
  val backupTarget: Option[InetSocketAddress]

  def receive: Receive = {
    case UploadProxyActor.Events.UploadRequest(key, source) ⇒
      val in = new PipedInputStream(4 * 1024 * 1024)
      val out = new PipedOutputStream()
      // ask Pipeline actor to handle source → out → in
      pipelineStreamActor.ask(PipeLineStreamActor.Events.PipeLineStream(in, out, source))
      // ask Put actor to put in → ufs3
      val f2 = putActorRef(coreConfig, backupTarget).ask(PutActor.Events.Put(key, in))
      import context.dispatcher

      val _sender = sender()

      f2.onComplete {
        case Success(x) ⇒
          _sender ! x
          in.close()
          out.close()

        case Failure(t) ⇒
          _sender ! Some(t)
          in.close()
          out.close()
      }

  }
}
object UploadProxyActor {
  object Events {
    final case class UploadRequest(key: String, source: Source[ByteString, Any])
  }

  def props(_coreConfig: CoreConfig, _backupTarget: Option[InetSocketAddress]): Props = {
    Props(new UploadProxyActor {
      val coreConfig: CoreConfig = _coreConfig
      val backupTarget: Option[InetSocketAddress] = _backupTarget
    })
  }

  //smart constructor
  def uploadProxyActorRef(coreConfig: CoreConfig, backupTarget: Option[InetSocketAddress])(implicit system: ActorSystem): ActorRef = system.actorOf(props(coreConfig, backupTarget))
}

trait PipeLineStreamActor extends Actor{
  import PipeLineStreamActor._

  implicit val materializer = ActorMaterializer()
  def receive: Receive = {
    case Events.PipeLineStream(in, out, source) ⇒
      out.connect(in)
      val sink = StreamConverters.fromOutputStream(() ⇒ out)
      //todo use another blocking ec
      import context.dispatcher
      val _sender = sender()
      source.runWith(sink).map(ioResult ⇒ {
        _sender ! 0
        context.stop(self)
      }).recover{
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
  val coreConfig: CoreConfig
  val backupTarget: Option[InetSocketAddress]

  val ufs3: AtomicReference[UFS3] = new AtomicReference[Data.UFS3]()
  val backupThread = new AtomicReference[Option[BackupSingleThread]](None)


  override def preStart(): Unit = {
    super.preStart()
    ufs3.set(PutCommand.writableUfs3(coreConfig))
    if (backupTarget.isDefined) {
      val backup = BackupSingleThread(coreConfig, backupTarget)
      backup.start()
      backupThread.set(Some(backup))
    }
  }

  def receive: Receive = {
    case Events.Put(key, ins) ⇒
      val _sender = sender()

      PutCommand._runWithUfs3(coreConfig, key, ins, ufs3.get()) match {
        case Success(_) ⇒ // println("put ok")
          _sender ! None
        case Failure(x) ⇒ //x.printStackTrace()
          _sender ! Some(x)
      }

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

  def props(_coreConfig: CoreConfig, _backupTarget: Option[InetSocketAddress]): Props = Props(new PutActor {
    val coreConfig: CoreConfig = _coreConfig
    val backupTarget: Option[InetSocketAddress] = _backupTarget
  })
  def putActorRef(coreConfig: CoreConfig, _backupTarget: Option[InetSocketAddress])(implicit ac: ActorContext): ActorRef = ac.actorOf(props(coreConfig, _backupTarget))
}