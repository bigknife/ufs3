package ufs3
package audit
package interprer

import java.util.concurrent.atomic.AtomicReference

import cats.data.Kleisli
import cats.effect.IO
import infr.{JsonFormat, RMSupport}
import ufs3.audit.interprer.AuditInterpreter.Config
import ufs3.kernel.audit.Audit

import scala.concurrent.ExecutionContext

/**
  * Created by songwenchao on 2017/7/17.
  */
trait AuditInterpreter extends Audit.Handler[Kleisli[IO, Config, ?]] {
  import spray.json._
  private lazy val uriRef = new AtomicReference[String]
  private lazy val rmSupport = new RMSupport {
    override val mongoConnUri: String = uriRef.get()
  }
  override def begin(auditInfo: Audit.BeginAudit): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    import rmSupport._
    uriRef.set(config.mongoUri)
    IO {
      implicit val auditJsonFormat = JsonFormat.BeginAuditFormat
      insertInto(config.collectionName, auditInfo.toJson)
    }
  }

  override def process(auditInfo: Audit.ProcessAudit): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    import rmSupport._
    uriRef.set(config.mongoUri)
    IO {
      implicit val auditJsonFormat = JsonFormat.ProcessAuditFormat
      insertInto(config.collectionName, auditInfo.toJson)
    }
  }

  override def end(auditInfo: Audit.EndAudit): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    import rmSupport._
    uriRef.set(config.mongoUri)
    IO {
      implicit val auditJsonFormat = JsonFormat.EndAuditFormat
      insertInto(config.collectionName, auditInfo.toJson)
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
