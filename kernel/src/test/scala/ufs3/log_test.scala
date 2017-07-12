package ufs3
package kernel
package test

import cats.Id
import cats.data.Coproduct
import cats.free.Free

import sop._
object LogTestApp extends App {
  import ufs3.kernel.log._
  import ufs3.kernel.audit._

  val log = Log[Log.Op]
  val logHandler = new Log.Handler[Id] {
    override protected[this] def debug(msg: String, cause: Option[Throwable]): Id[Unit] = {
      println(msg)
      if (cause.isDefined) cause.get.printStackTrace()
    }
    override protected[this] def info(msg: String, cause: Option[Throwable]): Id[Unit] = {
      println(msg)
      if (cause.isDefined) cause.get.printStackTrace()
    }
    override protected[this] def warn(msg: String, cause: Option[Throwable]): Id[Unit] = {
      println(msg)
      if (cause.isDefined) cause.get.printStackTrace()
    }
    override protected[this] def error(msg: String, cause: Option[Throwable]): Id[Unit] = {
      println(msg)
      if (cause.isDefined) cause.get.printStackTrace()
    }
  }
  val auditHandler = new Audit.Handler[Id] {
    override protected[this] def begin(auditInfo: BeginAudit): Id[Unit] = println(auditInfo)

    override protected[this] def process(auditInfo: ProcessAudit): Id[Unit] = println(auditInfo)

    override protected[this] def end(auditInfo: EndAudit): Id[Unit] = println(auditInfo)
  }
  log.info("hello,world", new IllegalArgumentException("only test")).foldMap(logHandler)

  println("--------------------------")

  type AuditLogApp[A] = Coproduct[Audit.Op, Log.Op, A]
  def program(implicit A: Audit[AuditLogApp], L: Log[AuditLogApp]): SOP[AuditLogApp, Unit] = {
    import A._, L._, AuditInfo._
    import java.util.Date
    for {
      _ ← debug("Hello, this is the freestyle-like program")
      _ ← begin(happening(new Date(), "testApp", "will info something"))
      _ ← info("step1 ....")
      _ ← process(processing(new Date(), "testApp", "process step1"))
      _ ← end(happyEnding(new Date, "testApp", "ended"))
    } yield ()
  }

  val auditLogInterpreter = auditHandler or logHandler
  program.foldMap(auditLogInterpreter)
}
