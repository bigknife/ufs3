package ufs3.integ

import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorRef
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Route
import fs2.Task
import pharaoh.SimplePharaohApp
import ufs3.world.commons.HttpServerArg
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Directives.{get => httpGet}
import akka.stream.IOResult
import akka.stream.scaladsl.{Source, StreamConverters}
import akka.util.ByteString
import ufs3.integ.http_server.actor.DownloadActor
import ufs3.interp.commons.Stack
import ufs3.kernel.commons.{Config, FileMode, UFS3}
import ufs3.kernel.modules.App
import ufs3.prog.open
import ufs3.prog._

import scala.concurrent.Future
import scala.util.control.NonFatal

object httpServer {

  def openUFS3(config: Config): UFS3 = {
    ufs3Holder
      .get()
      .orElse[UFS3] {
      val ufs3 = Some(Stack.parseApp[UFS3](open(FileMode.ReadOnly)).run(config).unsafeRun())
      ufs3Holder.set(ufs3)
      ufs3
    }
      .get

  }

  private [this] val ufs3Holder = new AtomicReference[Option[UFS3]](None)
  // get route
  def getRoute(config: Config, actor: ActorRef): Route = path("get" / Segment) { key ⇒
    httpGet{
      complete {
        //todo use another block ec
        import scala.concurrent.ExecutionContext.Implicits.global
        Future {
          def read = {
            val out = new java.io.PipedOutputStream()
            val in  = new java.io.PipedInputStream(4 * 1024 * 1024)
            out.connect(in)
            actor ! DownloadActor.Download(config, key, out, in)
            /*
            new Thread(new Runnable {
              def run(): Unit = {
                out.write("hello,world".getBytes)
                out.close()
              }
            }).start()
            */
            val byteStringSource: Source[ByteString, Future[IOResult]] =
              StreamConverters.fromInputStream(in = () ⇒ {
                //controll reading count
                in
              })

            val entity = HttpEntity.Chunked.fromData(ContentTypes.`application/octet-stream`, chunks = byteStringSource)
            HttpResponse(status = StatusCodes.OK, entity = entity)
          }

          val existed = Stack.parseApp(existedKey[App.Op](key, openUFS3(config))).run(config).unsafeRun()
          if (existed) read
          else HttpResponse(status = StatusCodes.NotFound, entity = s"$key not found")
        }
      }
    }
  }

  def apply(arg: HttpServerArg): Task[Unit] = {
    Task.delay {
      val serverApp = new SimplePharaohApp {

        override lazy val welcome: String = """
                                              |    __  __________________
                                              |  / / / / ____/ ___/__  /
                                              | / / / / /_   \__ \ /_ <
                                              |/ /_/ / __/  ___/ /__/ /
                                              |\____/_/    /____/____/
                                              |""".stripMargin

        import pharaoh.SimplePharaohApp._

        override lazy val extExceptionHandler: PartialFunction[Throwable, Route] = {
          case NonFatal(x) ⇒
            x.printStackTrace()
            complete(HttpResponse(status = StatusCodes.InternalServerError, entity = x.getMessage))
        }
      }
      import serverApp.system

      val getActor = serverApp.system.actorOf(DownloadActor.props)
      serverApp.register(getRoute(arg.asConfig, getActor))

      //if (mode == "read-write") {
      //  val putActor = UploadProxyActor.uploadProxyActorRef(config, backupTarget)(serverApp.system)
      //  serverApp.register(putRoute(config, putActor))
      //}
      import scala.concurrent.ExecutionContext.Implicits.global
      serverApp.listen(arg.host, arg.port) onSuccess {
        case _ ⇒ println(s"server is listen ${arg.host}:${arg.port}")
      }
      ()
    }
  }
}