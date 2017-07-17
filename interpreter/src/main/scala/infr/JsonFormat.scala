package infr

import spray.json.{DefaultJsonProtocol, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}
import ufs3.kernel.audit.Audit.{AuditInfo, BeginAudit, EndAudit, ProcessAudit}

/**
  * Created by songwenchao on 2017/7/7.
  */
object JsonFormat {

  implicit object BeginAuditJsonFormat extends RootJsonFormat[BeginAudit] {
    override def read(json: JsValue): BeginAudit = {
      val fields = json.asJsObject.fields
      val time   = fields("time").asInstanceOf[JsNumber].value.longValue()
      val app    = fields("app").asInstanceOf[JsString].value
      val msg    = fields("msg").asInstanceOf[JsString].value
      BeginAudit(time, app, msg)
    }

    override def write(obj: BeginAudit): JsValue = JsObject(
      "time" → JsNumber(obj.time.getTime),
      "app"  → JsNumber(obj.app),
      "msg"  → JsString(obj.msg)
    )
  }
  implicit object ProcessAuditJsonFormat extends RootJsonFormat[ProcessAudit] {
    override def read(json: JsValue): ProcessAudit = {
      val fields = json.asJsObject.fields
      val time   = fields("time").asInstanceOf[JsNumber].value.longValue()
      val app    = fields("app").asInstanceOf[JsString].value
      val msg    = fields("msg").asInstanceOf[JsString].value
      ProcessAudit(time, app, msg)
    }

    override def write(obj: ProcessAudit): JsValue = JsObject(
      "time" → JsNumber(obj.time.getTime),
      "app"  → JsNumber(obj.app),
      "msg"  → JsString(obj.msg)
    )
  }
  implicit object EndAuditJsonFormat extends RootJsonFormat[EndAudit] {
    override def read(json: JsValue): EndAudit = {
      val fields = json.asJsObject.fields
      val time   = fields("time").asInstanceOf[JsNumber].value.longValue()
      val app    = fields("app").asInstanceOf[JsString].value
      val msg    = fields("msg").asInstanceOf[JsString].value
      EndAudit(time, app, msg)
    }

    override def write(obj: EndAudit): JsValue = JsObject(
      "time" → JsNumber(obj.time.getTime),
      "app"  → JsNumber(obj.app),
      "msg"  → JsString(obj.msg)
    )
  }

}
