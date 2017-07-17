package infr

import spray.json.{JsNumber, JsObject, JsString, JsValue, RootJsonFormat}
import ufs3.kernel.audit.Audit.{AuditInfo, BeginAudit, EndAudit, ProcessAudit}

/**
  * Created by songwenchao on 2017/7/7.
  */
object JsonFormat {

  trait AuditInfoFormat[T <: AuditInfo] extends RootJsonFormat[T] {
    override def write(obj: T): JsValue = JsObject(
      "time" → JsNumber(obj.time.getTime),
      "app"  → JsNumber(obj.app),
      "msg"  → JsString(obj.msg)
    )
//    override def read(json: JsValue): T = {
//      val fields = json.asJsObject.fields
//      val time   = fields("time").asInstanceOf[JsNumber].value.longValue()
//      val app    = fields("app").asInstanceOf[JsString].value
//      val msg    = fields("msg").asInstanceOf[JsString].value
//      AuditInfo(time, app, msg)
//    }

    override def read(json: JsValue): T = ???
  }

  implicit object BeginAuditFormat   extends AuditInfoFormat[BeginAudit]
  implicit object ProcessAuditFormat extends AuditInfoFormat[ProcessAudit]
  implicit object EndAuditFormat     extends AuditInfoFormat[EndAudit]

}
