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

import java.util.Date
import cats.free.Inject
import scala.language.higherKinds
import scala.language.implicitConversions
import sop._

/**
  * Audit Free Monad
  */
sealed trait Audit[F[_]] {
  import Audit._

  def begin(auditInfo: BeginAudit): Par[F, Unit]
  def process(auditInfo: ProcessAudit): Par[F, Unit]
  def end(auditInfo: EndAudit): Par[F, Unit]
}
object Audit {

  sealed trait Op[A]

  case class Begin(auditInfo: BeginAudit) extends Op[Unit]

  case class Process(auditInfo: ProcessAudit) extends Op[Unit]

  case class End(auditInfo: EndAudit) extends Op[Unit]

  class To[F[_]](implicit I: Inject[Op, F]) extends Audit[F] {
    def begin(auditInfo: BeginAudit): Par[F, Unit] = liftPar_T[Op, F, Unit](Begin(auditInfo))

    def process(auditInfo: ProcessAudit): Par[F, Unit] = liftPar_T[Op, F, Unit](Process(auditInfo))

    def end(auditInfo: EndAudit): Par[F, Unit] = liftPar_T[Op, F, Unit](End(auditInfo))
  }

  implicit def to[F[_]](implicit I: Inject[Op, F]) = new To[F]

  def apply[F[_]](implicit A: Audit[F]): Audit[F] = A

  trait Handler[M[_]] extends NT[Op, M] {
    protected[this] def begin(auditInfo: BeginAudit): M[Unit]

    protected[this] def process(auditInfo: ProcessAudit): M[Unit]

    protected[this] def end(auditInfo: EndAudit): M[Unit]

    override def apply[A](fa: Op[A]): M[A] = fa match {
      case Begin(auditInfo)   ⇒ begin(auditInfo)
      case Process(auditInfo) ⇒ process(auditInfo)
      case End(auditInfo)     ⇒ end(auditInfo)
    }
  }

  /**
    * AuditInfo
    * ---------
    * Audit Info
    */
  sealed trait AuditInfo {
    def time: Date
    def app: String
    def msg: String
  }

  sealed trait BeginAudit   extends AuditInfo
  sealed trait EndAudit     extends AuditInfo
  sealed trait ProcessAudit extends AuditInfo

  object AuditInfo {

    case class Happening(time: Date, app: String, msg: String) extends BeginAudit

    case class HappyEnding(time: Date, app: String, msg: String) extends EndAudit

    case class SadEnding(time: Date, app: String, msg: String) extends EndAudit

    case class Processing(time: Date, app: String, msg: String) extends ProcessAudit

    def happening(time: Date, app: String, msg: String): BeginAudit = Happening(time, app, msg)

    def happyEnding(time: Date, app: String, msg: String): EndAudit = HappyEnding(time, app, msg)

    def sadEnding(time: Date, app: String, msg: String): EndAudit = SadEnding(time, app, msg)

    def processing(time: Date, app: String, msg: String): ProcessAudit = Processing(time, app, msg)

    def apply(_time: Long, _app: String, _msg: String): AuditInfo = new AuditInfo {
      override def time: Date  = new Date(_time)
      override def app: String = _app
      override def msg: String = _msg
    }
  }

}
