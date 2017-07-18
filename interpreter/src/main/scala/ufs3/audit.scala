package ufs3
package audit
package interprer

import java.util.concurrent.atomic.AtomicReference

import cats.data.Kleisli
import cats.effect.IO
import infr.{JsonFormat, RMSupport}
import ufs3.audit.interprer.AuditInterpreter.Config
import ufs3.interpreter.util.AtomicMap
import ufs3.kernel.audit.Audit

import scala.concurrent.ExecutionContext

/**
  * Created by songwenchao on 2017/7/17.
  */
trait AuditInterpreter extends Audit.Handler[Kleisli[IO, Config, ?]] {
  import spray.json._
  private lazy val atomicMap = AtomicMap[String, RMSupport]
  override def begin(auditInfo: Audit.BeginAudit): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    IO {
      val rmSupport = atomicMap(config.mongoUri) match {
        case Some(x) ⇒ x
        case None ⇒
          val newSupport = RMSupport()
          atomicMap += (config.mongoUri, newSupport)
          newSupport
      }
      implicit val auditJsonFormat = JsonFormat.BeginAuditFormat
      rmSupport
        .insertInto(config.collectionName, auditInfo.toJson)
        .local[Config](config ⇒ RMSupport.config(config.mongoUri))
    }
  }

  override def process(auditInfo: Audit.ProcessAudit): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    IO {
      val rmSupport = atomicMap(config.mongoUri) match {
        case Some(x) ⇒ x
        case None ⇒
          val newSupport = RMSupport()
          atomicMap += (config.mongoUri, newSupport)
          newSupport
      }
      implicit val auditJsonFormat = JsonFormat.ProcessAuditFormat
      rmSupport
        .insertInto(config.collectionName, auditInfo.toJson)
        .local[Config](config ⇒ RMSupport.config(config.mongoUri))
    }
  }

  override def end(auditInfo: Audit.EndAudit): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    IO {
      val rmSupport = atomicMap(config.mongoUri) match {
        case Some(x) ⇒ x
        case None ⇒
          val newSupport = RMSupport()
          atomicMap += (config.mongoUri, newSupport)
          newSupport
      }
      implicit val auditJsonFormat = JsonFormat.EndAuditFormat
      rmSupport
        .insertInto(config.collectionName, auditInfo.toJson)
        .local[Config](config ⇒ RMSupport.config(config.mongoUri))
    }
  }
}

object AuditInterpreter {

  trait Config {
    def mongoUri: String
    def collectionName: String

    def ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  }

  def config(_mongoUri: String, _collectionName: String): Config = new Config {
    override def collectionName: String = _collectionName

    override def mongoUri: String = _mongoUri
  }

  def apply(): AuditInterpreter = new AuditInterpreter {}
}
