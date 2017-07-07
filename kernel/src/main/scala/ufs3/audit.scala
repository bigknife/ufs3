/*
 * audit.scala
 * -----------
 * the kernel adt of audit
 * @author: bigknife@outlook.com
 * @create: 2017/07/03
 */

package ufs3
package kernel
package audit

import cats.free.{Free, Inject}
import Free.inject
import scala.language.higherKinds
import scala.language.implicitConversions

/**
  * Audit Free Monad
  */
sealed trait Audit[A]
object Audit {
  case class Begin(auditInfo: BeginAudit)     extends Audit[Unit]
  case class Process(auditInfo: ProcessAudit) extends Audit[Unit]
  case class End(auditInfo: EndAudit)         extends Audit[Unit]

  class Ops[F[_]](implicit I: Inject[Audit, F]) {
    def begin(auditInfo: BeginAudit): Free[F, Unit]     = inject[Audit, F](Begin(auditInfo))
    def process(auditInfo: ProcessAudit): Free[F, Unit] = inject[Audit, F](Process(auditInfo))
    def end(auditInfo: EndAudit): Free[F, Unit]         = inject[Audit, F](End(auditInfo))
  }
  object Ops {
    implicit def ops[F[_]](implicit I: Inject[Audit, F]) = new Ops[F]
  }
}

/**
  * AuditInfo
  * ---------
  * Audit Info
  */
sealed trait AuditInfo {
  def token: String
  def userId: String
  def appId: String
  def fileName: String
  def size: Long
  def mode: OperateMode
}

sealed trait BeginAudit   extends AuditInfo
sealed trait EndAudit     extends AuditInfo
sealed trait ProcessAudit extends AuditInfo

object AuditInfo {
  def apply(tk: String, uid: String, app: String, targetFile: String, relSize: Long, opMode: OperateMode): AuditInfo =
    new AuditInfo {
      override def token: String     = tk
      override def userId: String    = uid
      override def appId: String     = app
      override def fileName: String  = targetFile
      override def size: Long        = relSize
      override def mode: OperateMode = opMode
    }
}

sealed trait OperateMode {
  def description: String
}

object OperateMode {

  case object WriteData extends OperateMode {
    override def description: String = "write data"
  }

  case object ReadData extends OperateMode {
    override def description: String = "read data"
  }
}
