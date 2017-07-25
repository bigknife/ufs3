/**
  * command_serve.scala
  * ----------
  * get/put http server implementation
  * @author bigknife
  * @since 2017/7/24
  */
package ufs3
package integration
package command

import java.io.{InputStream, OutputStream}

import akka.actor.{Actor, ActorRef, Props}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Route
import pharaoh.SimplePharaohApp
import akka.http.scaladsl.server.Directives._
import akka.stream.IOResult
import akka.stream.scaladsl.{Source, StreamConverters}
import akka.util.ByteString
import ufs3.core.CoreConfig

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class DownloadActor extends Actor {

  def receive: Receive = {
    case DownloadActor.Download(config, key, out, in) ⇒
      GetCommand._run(config, key, out) match {
        case Success(_) ⇒
          out.close()
          in.close()

        case Failure(t) ⇒
          //todo add log
          t.printStackTrace()
          out.write(t.getMessage.getBytes("utf-8"))
          out.close()
          in.close()
      }
  }
}
object DownloadActor {
  def props: Props = Props(classOf[DownloadActor])

  sealed trait Command
  final case class Download(config: CoreConfig, key: String, out: OutputStream, in: InputStream) extends Command
}

trait ServeCommand extends SimplePharaohApp {

  // get route
  def getRoute(config: CoreConfig, actor: ActorRef): Route = path("get" / Segment) { key ⇒
    get {
      complete {
        //todo use another block ec
        import scala.concurrent.ExecutionContext.Implicits.global
        Future {
          def read = {
            val byteStringSource: Source[ByteString, Future[IOResult]] =
              StreamConverters.fromInputStream(in = () ⇒ {
                val out = new java.io.PipedOutputStream()
                val in = new java.io.PipedInputStream()
                out.connect(in)
                //todo add actor router
                //controll reading count
                actor ! DownloadActor.Download(config, key, out, in)
                in
              })
            val entity = HttpEntity.Chunked.fromData(ContentTypes.`application/octet-stream`, chunks = byteStringSource)
            HttpResponse(status = StatusCodes.OK, entity = entity)
          }

          GetCommand.existed(config, key) match {
            case Success(true) ⇒ read
            case Success(false) ⇒ HttpResponse(status = StatusCodes.NotFound, entity = s"$key not found")
            case Failure(t) ⇒ throw t
          }

        }
      }
    }
  }



  def run(config: CoreConfig, host: String, port: Int): Unit = {
    val serverApp = new SimplePharaohApp {
      override lazy val welcome: String = """
                                            |    __  __________________
                                            |  / / / / ____/ ___/__  /
                                            | / / / / /_   \__ \ /_ <
                                            |/ /_/ / __/  ___/ /__/ /
                                            |\____/_/    /____/____/
                                            |""".stripMargin

      import SimplePharaohApp._

      override lazy val extExceptionHandler: PartialFunction[Throwable, Route] = {
        case NonFatal(x) ⇒ complete(HttpResponse(status = StatusCodes.InternalServerError, entity = x.getMessage))
      }
    }
    val actor = serverApp.system.actorOf(DownloadActor.props)
    serverApp.register(getRoute(config, actor))
    serverApp.listen(host, port)

    ()
  }
}

object ServeCommand extends ServeCommand
