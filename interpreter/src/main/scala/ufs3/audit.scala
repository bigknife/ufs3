package ufs3
package audit
package interprer

import cats.data.Kleisli
import cats.effect.IO
import infr.{JsonFormat, RMSupport}
import ufs3.audit.interprer.AuditInterpreter.Config
import ufs3.kernel.audit.Audit

/**
  * Created by songwenchao on 2017/7/17.
  */
trait AuditInterpreter extends Audit.Handler[Kleisli[IO, Config, ?]] with RMSupport {
  override def begin(auditInfo: Audit.BeginAudit): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    IO {
      import spray.json._
      implicit val auditJsonFormat = JsonFormat.BeginAuditFormat
      insertInto(config.collectionName, auditInfo.toJson)
        .local[Config](config ⇒ RMSupport.config(config.mongoUri))
    }
  }

  override def process(auditInfo: Audit.ProcessAudit): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    IO {
      import spray.json._
      implicit val auditJsonFormat = JsonFormat.ProcessAuditFormat
      insertInto(config.collectionName, auditInfo.toJson)
        .local[Config](config ⇒ RMSupport.config(config.mongoUri))
    }
  }

  override def end(auditInfo: Audit.EndAudit): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    IO {
      import spray.json._
      implicit val auditJsonFormat = JsonFormat.EndAuditFormat
      insertInto(config.collectionName, auditInfo.toJson)
        .local[Config](config ⇒ RMSupport.config(config.mongoUri))
    }
  }
}

object AuditInterpreter {

  trait Config {
    def mongoUri: String
    def collectionName: String
  }

  def config(_mongoUri: String, _collectionName: String): Config = new Config {
    override def collectionName: String = _collectionName
    override def mongoUri: String       = _mongoUri
  }

  def apply(): AuditInterpreter = new AuditInterpreter {}
}
