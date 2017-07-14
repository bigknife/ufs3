package audit.infr

import spray.json.{DefaultJsonProtocol, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}
import ufs3.kernel.audit.Audit.AuditInfo

/**
  * Created by songwenchao on 2017/7/7.
  */
object JsonFormat {

  implicit object AuditInfoFormat extends DefaultJsonProtocol with RootJsonFormat[AuditInfo] {
    override def read(json: JsValue): AuditInfo = {
      val fields = json.asJsObject.fields
      val time   = fields("time").asInstanceOf[JsNumber].value.longValue()
      val app    = fields("app").asInstanceOf[JsString].value
      val msg    = fields("msg").asInstanceOf[JsString].value
      AuditInfo(time, app, msg)
    }

    override def write(obj: AuditInfo): JsValue = JsObject(
      "time" → JsNumber(obj.time.getTime),
      "app"  → JsNumber(obj.app),
      "msg"  → JsString(obj.msg)
    )
  }

}
