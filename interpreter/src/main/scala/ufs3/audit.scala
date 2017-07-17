package ufs3
package audit
package interprer

import cats.data.Kleisli
import cats.effect.IO
import ufs3.audit.interprer.AuditInterpreter.Config
import ufs3.kernel.audit.Audit

import scala.concurrent.Future

/**
  * Created by songwenchao on 2017/7/17.
  */
trait AuditInterpreter extends Audit.Handler[Kleisli[IO, Config, Future[Unit]]]{
  override def begin(auditInfo: Audit.BeginAudit): Kleisli[IO,Config,Future[Unit]] =
}

object AuditInterpreter {

  trait Config {
    def mongoUri:String
    def collectionName:String
  }

  def config(_mongoUri:String,_collectionName:String):Config=new Config {
    override def collectionName: String =_collectionName

    override def mongoUri: String =_mongoUri
  }

  def apply(): AuditInterpreter = new AuditInterpreter {}
}
