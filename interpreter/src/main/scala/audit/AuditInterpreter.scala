package audit

import audit.infr.{JsonFormat, RMSupport}
import ufs3.kernel.audit.Audit
import ufs3.kernel.audit.Audit.AuditInfo

import scala.concurrent.Future

/**
  * Created by songwenchao on 2017/7/7.
  */
object AuditInterpreter extends RMSupport with Audit.Handler[Future] {

  override val mongoUri: String   = ""
  private val auditCollectionName = "auditInfo"

  implicit val auditInfoJsonFormat = JsonFormat.AuditInfoFormat
  import spray.json._

  private def store(auditInfo: AuditInfo): Future[Unit] = insertInto(auditCollectionName, auditInfo.toJson)

  override protected[this] def begin(auditInfo: Audit.BeginAudit): Future[Unit]     = store(auditInfo)
  override protected[this] def process(auditInfo: Audit.ProcessAudit): Future[Unit] = store(auditInfo)
  override protected[this] def end(auditInfo: Audit.EndAudit): Future[Unit]         = store(auditInfo)
}
