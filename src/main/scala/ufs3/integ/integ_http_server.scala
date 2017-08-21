package ufs3.integ

import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorRef
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Route
import fs2.Task
import pharaoh.SimplePharaohApp
import ufs3.world.commons.HttpServerArg
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Directives.{get => httpGet, put => httpPut}
import akka.stream.IOResult
import akka.stream.scaladsl.{Source, StreamConverters}
import akka.util.{ByteString, Timeout}
import ufs3.integ.http_server.actor.{DownloadActor, UploadProxyActor}
import ufs3.interp.commons.Stack
import ufs3.kernel.commons.{Config, FileMode, UFS3}
import ufs3.kernel.modules.App
import ufs3.prog.open
import ufs3.prog._
import akka.pattern._

import scala.concurrent.Future
import scala.util.control.NonFatal

object httpServer {

  def openReadWriteUFS3(config: Config): UFS3 = {
    ufs3WritableHolder
      .get()
      .orElse[UFS3] {
      val ufs3 = Some(Stack.parseApp[UFS3](open(FileMode.ReadWrite)).run(config).unsafeRun())
      ufs3WritableHolder.set(ufs3)
      ufs3
    }
      .get
  }

  def openReadonlyUFS3(config: Config): UFS3 = {
    ufs3ReadonlyHolder
      .get()
      .orElse[UFS3] {
      val ufs3 = Some(Stack.parseApp[UFS3](open(FileMode.ReadOnly)).run(config).unsafeRun())
      ufs3ReadonlyHolder.set(ufs3)
      ufs3
    }
      .get
  }

  private [this] val ufs3WritableHolder = new AtomicReference[Option[UFS3]](None)
  private [this] val ufs3ReadonlyHolder = new AtomicReference[Option[UFS3]](None)
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
            val byteStringSource: Source[ByteString, Future[IOResult]] =
              StreamConverters.fromInputStream(in = () ⇒ {
                //controll reading count
                in
              })

            val entity = HttpEntity.Chunked.fromData(ContentTypes.`application/octet-stream`, chunks = byteStringSource)
            HttpResponse(status = StatusCodes.OK, entity = entity)
          }

          val existed = Stack.parseApp(existedKey[App.Op](key, openReadWriteUFS3(config))).run(config).unsafeRun()
          if (existed) read
          else HttpResponse(status = StatusCodes.NotFound, entity = s"$key not found")
        }
      }
    }
  }

  // put route
  def putRoute(config: Config, actor: ActorRef): Route = path("put" / Segment) { key ⇒
    httpPut {
      extractRequestContext { ctx ⇒
        fileUpload("file") {
          case (fileInfo, byteSource) ⇒
            complete {
              def putFile(): Future[HttpResponse] = {
                import scala.concurrent.duration._
                implicit val timeout: Timeout = Timeout(24.hours)
                val f                = actor ? UploadProxyActor.Events.UploadRequest(key, byteSource)
                // f will be Option[Throwable]
                import scala.concurrent.ExecutionContext.Implicits.global
                f.map {
                  case Some(t: Throwable) ⇒
                    t.printStackTrace()
                    HttpResponse(status = StatusCodes.InternalServerError, entity = t.getMessage)
                  case _ ⇒ HttpResponse(status = StatusCodes.OK, entity = s"$key is put")
                }
              }

              val ufs3 = openReadWriteUFS3(config)
              val existed = Stack.parseApp(existedKey[App.Op](key, ufs3)).run(config).unsafeRun()
              if (existed) {
                Future.successful[HttpResponse](
                  HttpResponse(status = StatusCodes.Conflict, entity = s"$key has existed")
                )
              }else {
                putFile()
              }
            }
        }
      }
    }
  }

  def apply(arg: HttpServerArg): Task[Unit] = {
    Task.delay {

      def str2InetSocketAddress(str: String):InetSocketAddress = str.split(":") match {
        case Array(host, port) ⇒ new InetSocketAddress(host, port.toInt)
        case _ ⇒ throw new IllegalArgumentException("socket address should be host:port")
      }

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

      if (arg.mode == "read-write") {
        val putActor = UploadProxyActor.uploadProxyActorRef(arg.asConfig, arg.backupServer.map(str2InetSocketAddress))(serverApp.system)
        serverApp.register(putRoute(arg.asConfig, putActor))
      }
      import scala.concurrent.ExecutionContext.Implicits.global
      serverApp.listen(arg.host, arg.port) onSuccess {
        case _ ⇒ println(s"server is listen ${arg.host}:${arg.port}")
      }
      ()
    }

  }
}