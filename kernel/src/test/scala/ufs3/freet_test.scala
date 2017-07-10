package ufs3
package kernel
package test

import cats._
import cats.implicits._
import cats.free._
import block._
import cats.data.Kleisli

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

sealed trait Sample[A]
object Sample {
  type Response[A] = Either[Throwable, A]
  type Async[A] = Kleisli[Future, Unit, A]

  case class Think(sth: String) extends Sample[Int]
  case class Doit(sth: String) extends Sample[Boolean]
  case class Done(sth: String) extends Sample[Boolean]
  case class Blala(sth: String) extends Sample[Unit]
}

object FreeTTestApp extends App {
  import Sample._
  import scala.concurrent.ExecutionContext.Implicits.global

  val ftThing: FreeT[Sample, Async, Int] = FreeT.liftF(Think("WhoIAm"))
  val ftDoit = FreeT.liftF[Sample, Async, Boolean](Doit("nonono!"))
  val ftDone = FreeT.liftF[Sample, Async, Boolean](Done("yesyesyes!"))
  val blala =  FreeT.liftF[Sample, Async, Unit](Blala("yesyesyes!"))

  val interpreter = new (Sample ~> Response) {
    override def apply[A](fa: Sample[A]): Response[A] = fa match {
      case Think(sth) ⇒ Right({println("We are thinking about something"); 1}): Response[Int]
      case Doit(sth) ⇒ Left(new IllegalArgumentException("We only think, don't do it")): Response[Boolean]
      case Done(sth) ⇒ Right({println("we've done it");true})
      case Blala(sth) ⇒ Right(println("blalala!!! "))
    }
  }

  val intp1 = new (Sample ~> Future) {
    override def apply[A](fa: Sample[A]): Future[A] = fa match {
      case Think(sth) ⇒ Future{println(s"${Thread.currentThread()}think in future");Thread.sleep(1000); 1}
      case Doit(sth) ⇒ Future{throw new IllegalArgumentException("doit in future")}
      case Done(sth) ⇒ Future{println(s"${Thread.currentThread()}all be done in future");Thread.sleep(1000); true}
      case Blala(sth) ⇒ Future{println(s"${Thread.currentThread()}blalba in future"); Thread.sleep(1000)}
    }
  }
  val intp2 = new (Future ~> Response) {
    override def apply[A](fa: Future[A]): Response[A] = Try{Await.result(fa, Duration.Inf)} match {
      case scala.util.Failure(t) ⇒ Left(t): Response[A]
      case scala.util.Success(a) ⇒ Right(a): Response[A]
    }

  }
  val intpl3 = intp1.andThen(intp2)

  def f(x: Int) = for {
    i ← ftThing
    a ← if (i == x) ftDone else ftDoit
    _ ← blala
  } yield a

  f(0).hoist()



  //f(1).compile(interpreter)

  val intp4 = new (Sample ~> Async) {
    override def apply[A](fa: Sample[A]): Async[A] = fa match {
      case Think(sth) ⇒ Kleisli {
        case () ⇒
        Future {
          println(s"${Thread.currentThread()} $sth")
          Thread.sleep(1000)
          1
        }
      }
      case Doit(sth) ⇒ Kleisli {
        case () ⇒
        Future {
          println(s"${Thread.currentThread()} do it")
          Thread.sleep(1000)
          throw new RuntimeException("do it allways throw exception")
        }
      }
      case Done(sth) ⇒ Kleisli {
        case () ⇒
        Future {
          println(s"${Thread.currentThread()} done")
          true
        }
      }
      case Blala(sth) ⇒ Kleisli {
        case () ⇒
        Future {
          println(s"${Thread.currentThread()} blala")
        }
      }
    }
  }

  val future = f(0).foldMap(intp4).run(())
  println(Await.result(future, Duration.Inf))

}