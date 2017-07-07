package audit.infr

import spray.json.{DefaultJsonProtocol, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}
import ufs3.kernel.audit.{AuditInfo, OperateMode}

/**
  * Created by songwenchao on 2017/7/7.
  */
object JsonFormat {

  implicit object AuditInfoFormat extends DefaultJsonProtocol with RootJsonFormat[AuditInfo] {
    override def read(json: JsValue): AuditInfo = {
      val fields   = json.asJsObject.fields
      val token    = fields("token").asInstanceOf[JsString].value
      val userId   = fields("userId").asInstanceOf[JsString].value
      val fileName = fields("fileName").asInstanceOf[JsString].value
      val appId    = fields("appId").asInstanceOf[JsString].value
      val size     = fields("size").asInstanceOf[JsNumber].value.longValue
      val mode     = fields("mode").asJsObject
      AuditInfo(token, userId, appId, fileName, size, ModeFormat.read(mode))
    }

    override def write(obj: AuditInfo): JsValue = JsObject(
      "token"    → JsString(obj.token),
      "userId"   → JsString(obj.userId),
      "fileName" → JsString(obj.fileName),
      "appId"    → JsString(obj.appId),
      "size"     → JsNumber(obj.size),
      "mode"     → ModeFormat.write(obj.mode)
    )
  }

  implicit object ModeFormat extends DefaultJsonProtocol with RootJsonFormat[OperateMode] {
    override def read(json: JsValue): OperateMode = {
      val fields = json.asJsObject.fields
      val desc   = fields("description").asInstanceOf[JsString].value
      if (desc.contains("read")) OperateMode.ReadData
      else OperateMode.WriteData
    }
    override def write(obj: OperateMode): JsValue = JsObject("description" → JsString(obj.description))
  }
}
