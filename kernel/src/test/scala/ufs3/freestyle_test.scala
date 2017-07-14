package ufs3
package kernel
package test
import cats.arrow.FunctionK
import cats.{Monad, ~>}
import cats.free.{Free, FreeApplicative, Inject}
import ufs3.kernel.test.FreeStyleTestApp.Validation.HasNumber
import cats.implicits._
import cats.~>
import cats.Id
import cats.data.{Coproduct, Prod}
import ufs3.kernel.test.FreeStyleTestApp.Log.All

import scala.language.higherKinds

object FreeStyleTestApp extends App {

  type FreeS[F[_], A] = Free[FreeApplicative[F, ?], A]
  type Par[F[_], A] = FreeApplicative[F, A]
  type SeqPar[F[_], A] = FreeS[F, A]

  def liftPar[F[_], A](par: FreeApplicative[F[?], A]): FreeS[F, A] =
    Free.liftF[FreeApplicative[F, ?], A](par)

  //implicit def monadForFreeS[F[_]]: Monad[FreeS[F, ?]] =
    //Free.catsFreeMonadForFree[FreeApplicative[F, ?]]



  trait Validation[F[_]] {
    def hasNumber(str: String): Par[F, Boolean]
  }
  object Validation {
    sealed trait Op[A]
    final case class HasNumber(str: String) extends Op[Boolean]

    class To[F[_]](implicit I: Inject[Op, F]) extends Validation[F] {
      override def hasNumber(str: String): Par[F, Boolean] = {
        val fa = FreeApplicative.lift[Op, Boolean](HasNumber(str))
        fa.compile[F](new (Op ~> F){
          override def apply[A](fa: Op[A]): F[A] = I.inj(fa)
        })
      }
    }

    implicit def to[F[_]](implicit I: Inject[Op, F]): To[F] = new To[F]
    def apply[F[_]](implicit V: Validation[F]): Validation[F] = V
  }

  val validation = Validation.to[Validation.Op] //Validation[Validation.Op]()

  //println(monadForFreeS[Validation.Op])

  val p: Par[Validation.Op, Boolean] = for {
    x ← validation.hasNumber("128")
  } yield x

  println(p.foldMap(new (Validation.Op ~> Id) {
    override def apply[A](fa: Validation.Op[A]): Id[A] = fa match {
      case HasNumber(str) ⇒ println(s"str is $str"); "0123456789".contains(str)
    }
  }))

  trait Log[F[_]] {
    def all(str: String): Par[F, Unit]
  }
  object Log {
    sealed trait Op[A]
    final case class All(str: String) extends Op[Unit]

    class To[F[_]](implicit I: Inject[Op, F]) extends Log[F] {
      def all(str: String): Par[F, Unit] = {
        val fa = FreeApplicative.lift[Op, Unit](All(str))
        fa.compile(new (Op ~> F) {
          override def apply[A](fa: Op[A]): F[A] = I.inj(fa)
        })
      }
    }

    implicit def to[F[_]](implicit I: Inject[Op, F]) = new To[F]
    def apply[F[_]](implicit L: Log[F]) = L
  }





  println("--------------------")
  val log = Log[Log.Op]
  println(log.all("hello,world"))

  implicit def monadForFreeS[F[_]]: Monad[FreeS[F, ?]] =
    Free.catsFreeMonadForFree[FreeApplicative[F, ?]]

  type BigApp[A] = Coproduct[Log.Op, Validation.Op, A]

  //println(Log.to[BigApp])

  val blog = Log[BigApp]
  val bv = Validation[BigApp]

  //val l1: FreeApplicative[BigApp, Unit] = blog.all("hello")
  //val v1: FreeApplicative[BigApp, Boolean] = bv.hasNumber("hello,world")

  //println(Monad[FreeApplicative[BigApp, ?]])

  //l1.flatMap({case () ⇒ v1})

  //(l1 |@| v1).map((x, y) ⇒ ())


  //blog.all("hi").flatMap(_ ⇒ bv.hasNumber("123"))


  val s  = for {
    _ ← liftPar(blog.all("all log"))
    a ← liftPar(bv.hasNumber("abcdefg"))
  } yield a

  val logInterpreter = new (FreeApplicative[Log.Op,?] ~> Id) {
    override def apply[A](fa: Par[Log.Op, A]): Id[A] = ???
  }
  val valInterperter = new (FreeApplicative[Log.Op, ?]  ~> Id) {
    override def apply[A](fa: Par[Log.Op, A]): Id[A] = ???
  }

//  val interpreter = new (Par[BigApp, ?] ~> Id) {
//    override def apply[A](fa: Par[BigApp, A]): Id[A] =
//  }

  //s.foldMap(logInterpreter or valInterperter)



  //println(s)

  //val f1: FreeApplicative[Validation.Op, Boolean] = ???
  //f1.flatMap()

}