package audit

import audit.infr.{JsonFormat, RMSupport}
import cats.{Id, ~>}
import ufs3.kernel.audit.{Audit, AuditInfo}
import ufs3.kernel.audit.Audit.{Begin, End, Process}

import scala.concurrent.Await

/**
  * Created by songwenchao on 2017/7/7.
  */
object AuditInterpreter extends RMSupport {

  override val mongoUri: String   = ""
  private val auditCollectionName = "auditInfo"

  val auditInterpreter: Audit ~> Id = new (Audit ~> Id) {
    override def apply[A](fa: Audit[A]): Id[A] = fa match {
      case Begin(auditInfo) ⇒
        store(auditInfo)
      case Process(auditInfo) ⇒
        store(auditInfo)
      case End(auditInfo) ⇒
        store(auditInfo)
    }
  }

  implicit val auditInfoJsonFormat = JsonFormat.AuditInfoFormat
  import scala.concurrent.duration._
  import spray.json._
  private def store(auditInfo: AuditInfo): Unit =
    Await.result(insertInto(auditCollectionName, auditInfo.toJson), 5.seconds)

}
