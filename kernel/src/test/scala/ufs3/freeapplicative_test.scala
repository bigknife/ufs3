package ufs3
package kernel
package test
import java.text.SimpleDateFormat

import cats.arrow.FunctionK
import cats.data.{Const, Kleisli}

import scala.language.higherKinds
import cats.free.FreeApplicative
import cats.~>
import cats.implicits._

import scala.concurrent.Future

object FreeApplicativeTest extends App {
  sealed trait Validation {
    import Validation._
    def size(size: Int): FreeApplicative[Op, Boolean]
    def hasNumber: FreeApplicative[Op, Boolean]
  }

  object Validation {
    sealed trait Op[A] extends Product with Serializable
    final case class Size(size: Int) extends Op[Boolean]
    final case object HasNumber extends Op[Boolean]

    class Ops extends Validation {
      import FreeApplicative._
      override def size(size: Int): FreeApplicative[Op, Boolean] = lift(Size(size))

      override def hasNumber: FA[Op, Boolean] = lift(HasNumber)
    }

    def apply(): Validation = new Ops
  }

  val validation = Validation()
  import validation._
  import cats.implicits._
  type FromString[A] = String ⇒ A
  val prog: FreeApplicative[Validation.Op, Boolean] = (size(5) |@| hasNumber).map{case (l,r) ⇒ l && r}
  val compiler = new (Validation.Op ~> FromString) {
    import Validation._
    override def apply[A](fa: Validation.Op[A]): FromString[A] = str ⇒ {
      fa match {
        case Size(size) ⇒
          println(s"current thread ${Thread.currentThread().getName}")
          str.length >= size
        case HasNumber ⇒
          println(s"current thread ${Thread.currentThread().getName}")
          str.exists(c ⇒ "0123456789".contains(c))
      }
    }
  }
  type ParValidator[A] = Kleisli[Future, String, A]
  val parCompiler = new (Validation.Op ~> ParValidator) {
    import Validation._
    import scala.concurrent.ExecutionContext.Implicits.global
    override def apply[A](fa: Validation.Op[A]): ParValidator[A] = Kleisli {str ⇒
      fa match {
        case Size(size) ⇒ Future {
          println(s"size: current thread ${Thread.currentThread().getName}, current time: ${new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date)}")
          Thread.sleep(1000)
          str.length >= size
        }
        case HasNumber ⇒ Future {
          println(s"hasNumber: current thread ${Thread.currentThread().getName}, current time: ${new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date)}")
          Thread.sleep(1000)
          str.exists(c ⇒ "0123456789".contains(c))
        }
      }
    }
  }
  val validator = prog.foldMap(compiler)
  println(validator("1234"))
  println(validator("12345"))

  println("-----------------------------------")
  /*
  import cats._
  implicit apForFuture = new Applicative[ParValidator] {

    override def pure[A](x: A): ParValidator[A] = Kleisli {_ ⇒ Future {x}}

    override def ap[A, B](ff: ParValidator[(A) => B])(fa: ParValidator[A]): ParValidator[B] = ???
  }
  */
  import scala.concurrent.ExecutionContext.Implicits.global
  val parValidator = prog.foldMap[ParValidator](parCompiler)
  import scala.concurrent.Await
  import scala.concurrent.duration._
  println(Await.result(parValidator("123456"), Duration.Inf))

  println("-------------------------------------------")
  type Log[A] = Const[List[String], A]
  val logCompiler = new (Validation.Op ~> Log) {
    import Validation._
    override def apply[A](fa: Validation.Op[A]): Log[A] = fa match {
      case Size(size) ⇒ Const(List(s"size >= $size?"))
      case HasNumber ⇒ Const(List("has number?"))
    }
  }
  def logValidator[A](validator: FreeApplicative[Validation.Op, A]): List[String] =
    validator.foldMap[Log](logCompiler).getConst

  println(logValidator(prog))
  println(logValidator(size(5) *> hasNumber *> size(10)))

  println("___--------------------_________________--")
  import cats.data.Prod
  type ValidateAndLog[A] = Prod[ParValidator, Log, A]
  val prodCompiler: (Validation.Op ~> ValidateAndLog) = parCompiler and logCompiler
  val prodValidator = prog.foldMap(prodCompiler)
  println(Await.result(prodValidator.first("123456"), Duration.Inf))
  println(prodValidator.second.getConst)
}