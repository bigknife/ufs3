package sop
package test

import cats.Id
import cats.data.Kleisli
import cats.free.Inject
import sop._
import sop.test.Validation.{IsEmail, OverMaxLength}

import scala.concurrent.Await
import scala.language.higherKinds

// Log ADT
trait Log[F[_]] {
  def debug(msg: String): Par[F, Unit]
}
object Log {
  sealed trait Op[A]
  final case class Debug(msg: String) extends Op[Unit]

  class To[F[_]](implicit I: Inject[Op, F]) extends Log[F]{
    def debug(msg: String): Par[F, Unit] = liftPar_T[Op,F, Unit](Debug(msg))
  }
  implicit def to[F[_]](implicit I: Inject[Op, F]): Log[F] = new To[F]
  def apply[F[_]](implicit L: Log[F]): Log[F] = L
}

// Validation ADT
trait Validation[F[_]] {
  def isEmail(addr: String): Par[F, Boolean]
  def overMaxLength(str: String, maxLength: Int): Par[F, Boolean]
}
object Validation {
  sealed trait Op[A]
  final case class IsEmail(addr: String) extends Op[Boolean]
  final case class OverMaxLength(str: String, maxLength: Int) extends Op[Boolean]

  class To[F[_]](implicit I: Inject[Op, F]) extends Validation[F] {
    override def isEmail(addr: String): Par[F, Boolean] =
      liftPar_T[Op, F, Boolean](IsEmail(addr))

    override def overMaxLength(str: String, maxLength: Int): Par[F, Boolean] =
      liftPar_T[Op, F, Boolean](OverMaxLength(str, maxLength))
  }
  implicit def to[F[_]](implicit I: Inject[Op, F]): Validation[F] = new To[F]
  def apply[F[_]](implicit V: Validation[F]) = V
}


object sop_test extends App{
  sop_app.test()
}
object sop_app {
  import Log._
  val log: Log[Log.Op] = Log[Log.Op]
  val validation: Validation[Validation.Op] = Validation[Validation.Op]

  val logIdInterpreter = new NT[Log.Op, Id] {
    override def apply[A](fa: Log.Op[A]): Id[A] = fa match {
      case Debug(str) ⇒ println(s"[DEBUG] $str")
    }
  }
  val validationSeqInterpreter = new NT[Validation.Op, Id] {
    override def apply[A](fa: Validation.Op[A]): Id[A] = fa match {
      case IsEmail(addr) ⇒
        val ret = addr.contains("@") && addr.contains(".")
        println(s"$addr is email? $ret")
        Thread.sleep(1000)
        ret
      case OverMaxLength(addr, maxLength) ⇒
        val ret = addr.length <= maxLength
        println(s"$addr is over max length($maxLength)? $ret")
        Thread.sleep(1000)
        ret
    }
  }
  import java.text.{SimpleDateFormat => SDF}
  import java.util.Date

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  import scala.concurrent.duration._
  val validationParInterpreter = new NT[Validation.Op, Kleisli[Future, Any, ?]] {
    def now(): String = new SDF("yyyyMMdd-HH:mm:ss,SSS").format(new Date())
    override def apply[A](fa: Validation.Op[A]): Kleisli[Future, Any, A] = fa match {
      case IsEmail(addr) ⇒ Kleisli {x: Any ⇒
        Future {
          val ret = addr.contains("@") && addr.contains(".")
          println(s"${now()} validating IsEmail($addr): $ret")
          Thread.sleep(1000)
          ret
        }
      }
      case OverMaxLength(addr, maxLength) ⇒ Kleisli {x: Any ⇒
        Future {
          val ret = addr.length <= maxLength
          println(s"${now()} $addr is over max length($maxLength)? $ret")
          Thread.sleep(1000)
          ret
        }
      }
    }
  }

  def test(): Unit = {
    println("---------------TEST LOG---------------------")
    testLog()
    println("---------------Validation LOG SEQ---------------------")
    testSeqValidation()
    println("---------------Validation LOG PAR---------------------")
    testParValidation()
    println("--------------- Coproduct ----------------------")
    testCoproduct()
  }

  private[this] def testLog(): Unit = {
    import log._
    val program = for {
      _ ← debug("SOP 框架实例")
      _ ← debug("SOP is Sequencial Over Parallel")
      _ ← debug("SOP based on Free and FreeApplicative")
      _ ← debug("SOP 也是套路")
    } yield ()

    program.foldMap(logIdInterpreter)
  }

  private[this] def testSeqValidation(): Unit = {
    import validation._
    import cats.implicits._
    def validationProgram(str: String, maxLength: Int): Par[Validation.Op, Boolean] =
      (isEmail(str) |@| overMaxLength(str, maxLength)).map(_ && _)
    val ret: Id[Boolean] = validationProgram("songzenghui@gmail.com", 5).foldMap(validationSeqInterpreter)
    println(s"the final ret is $ret")
    println("--------use impilcit transformation to SOP------------")
    def lift2SopProgram(str: String, maxLength: Int): SOP[Validation.Op, Boolean] = {
      for {
        a ← isEmail(str)
        b ← overMaxLength(str, maxLength)
      } yield a && b
    }
    val ret1: Id[Boolean] = lift2SopProgram("songzenghui@gmail.com", 5).foldMap(validationSeqInterpreter)

    println(s"the final ret is $ret1")

  }

  private[this] def testParValidation(): Unit = {
    import cats.implicits._
    import validation._

    import scala.concurrent.ExecutionContext.Implicits.global
    case object Dummy

    def validationProgram(str: String, maxLength: Int): Par[Validation.Op, Boolean] =
      (isEmail(str) |@| overMaxLength(str, maxLength)).map(_ && _)
    val retFunc: Kleisli[Future, Any, Boolean] = validationProgram("songzenghui@gmail.com", 5).foldMap[Kleisli[Future, Any, ?]](validationParInterpreter)
    val retFuture = retFunc.run(Dummy)
    val ret = Await.result(retFuture, Duration.Inf)
    println(s"the final ret is $ret")
  }

  private[this] def testCoproduct(): Unit = {
    import cats.data.Coproduct
    type ValidateLogApp[A] = Coproduct[Validation.Op, Log.Op, A]
    val coLog = Log[ValidateLogApp]
    val coValidation = Validation[ValidateLogApp]
    import coLog._, coValidation._


    def validate(str: String, maxLength: Int): SOP[ValidateLogApp, Boolean] = for {
      _ ← debug(s"验证$str 是否是邮件，并确认是否超过最大长度")
      b ← isEmail(str)
      _ ← debug(s"验证结果：$b")
    } yield b

    val program = validate("songzenghui@gmail.com", 5)
    //val interpreter: NT[Par[ValidateLogApp, ?], Id] = (validationSeqInterpreter or logIdInterpreter): NT[ValidateLogApp, Id]
    val interpreter: NT[ValidateLogApp, Id] = validationSeqInterpreter or logIdInterpreter
    val b = program.foldMap(interpreter)
    println(s"final ret is $b")
  }
}

