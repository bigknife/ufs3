package ufs3
package audit
package interprer

import cats.data.Kleisli
import cats.effect.IO
import infr.{JsonFormat, RMSupport}
import ufs3.audit.interprer.AuditInterpreter.Config
import ufs3.kernel.audit.Audit
import sop._

/**
  * Created by songwenchao on 2017/7/17.
  */
trait AuditInterpreter extends Audit.Handler[Kleisli[IO, Config, ?]] with RMSupport {
  override def begin(auditInfo: Audit.BeginAudit): Kleisli[IO, Config, Resp[Unit]] = Kleisli { config ⇒
    import spray.json._
    implicit val auditJsonFormat = JsonFormat.BeginAuditFormat
    IO {
      try {
        val r = insertInto(config.collectionName, auditInfo.toJson).run(RMSupport.config(config.mongoUri)).unsafeRunSync()
        Right(r)
      } catch {
        case t: Throwable ⇒ Left(t)
      }

    }

  }

  override def process(auditInfo: Audit.ProcessAudit): Kleisli[IO, Config, Resp[Unit]] = Kleisli { config ⇒
    import spray.json._
    implicit val auditJsonFormat = JsonFormat.ProcessAuditFormat
    IO {
      try {
        val r = insertInto(config.collectionName, auditInfo.toJson)
          .run(RMSupport.config(config.mongoUri))
          .unsafeRunSync()
        Right(r)
      } catch {
        case t: Throwable ⇒ Left(t)
      }

    }

  }

  override def end(auditInfo: Audit.EndAudit): Kleisli[IO, Config, Resp[Unit]] = Kleisli { config ⇒
    import spray.json._
    implicit val auditJsonFormat = JsonFormat.EndAuditFormat
    IO {
      try {
        Right(insertInto(config.collectionName, auditInfo.toJson)
          .run(RMSupport.config(config.mongoUri))
          .unsafeRunSync())
      }catch {
        case t: Throwable ⇒ Left(t)
      }
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
