/**
  * RespSOPMonadTest.scala
  * ----------
  *
  * @author bigknife
  * @since 2017/8/4
  */
package ufs3
package core

package monad

import java.util.concurrent.atomic.AtomicLong

import cats.{Id, Monad}
import cats.free.Inject
import sop._
import ufs3.core.monad.SomeAction.{GetInt, Print}

import scala.language.higherKinds
import scala.util.Random

trait SomeAction[F[_]] {
  def getInt: Par[F, Resp[Int]]
  def print(str: String): Par[F, Resp[Unit]]
}

object SomeAction {
  sealed trait Op[A]
  case object GetInt                  extends Op[Resp[Int]]
  final case class Print(str: String) extends Op[Resp[Unit]]

  class To[F[_]](implicit I: Inject[Op, F]) extends SomeAction[F] {
    def getInt: Par[F, Resp[Int]] = liftPar_T[Op, F, Resp[Int]](GetInt)
    def print(str: String): Par[F, Resp[Unit]] = liftPar_T[Op, F, Resp[Unit]](Print(str))
  }
  implicit def to[F[_]](implicit I: Inject[Op, F]): SomeAction[F] = new To[F]

  def apply[F[_]](implicit S: SomeAction[F]) = S
}

object Test extends App {
  val action = SomeAction[SomeAction.Op]
  import action._

  /*
  val prog: RespSOP[SomeAction.Op, Int] = for {
    i ← getInt: Monad[RespSOP[SomeAction.Op, ?]]
    _ ← (if (i % 2 == 0) print(s"$i 是偶数") else print(s"$i 是奇数")):Monad[RespSOP[SomeAction.Op, Unit]]
  } yield i
  */
  val monad = RespSOPMonad[SomeAction.Op]
  val g = getInt: RespSOP[SomeAction.Op, Int]
  def f(str: String): RespSOP[SomeAction.Op, Unit] = print(str)
  val prog = monad.flatMap(g) { i ⇒
    print(s"i = $i")
  }

  val gops = toRespSOPMonad(g)
  import RespSOP._
  val prog1 = for {
    i ← getInt.asM
    _ ← (if (i % 2 == 0) f(s"$i 是偶数") else f(s"$i 是奇数")).asM
  } yield i

  val interpreter = new NT[SomeAction.Op, Id] {
    val count = new AtomicLong(0)
    def apply[A](fa: SomeAction.Op[A]): Id[A] = fa match {
      case GetInt ⇒
        Right(Random.nextInt())
      case Print(str) ⇒
        Right(println(str))
    }
  }


  prog.foldMap(interpreter) match {
    case Left(t)  ⇒ t.printStackTrace()
    case Right(i) ⇒ println(s"result is $i")
  }

  println("--------------")
  prog1.foldMap(interpreter) match {
    case Left(t)  ⇒ t.printStackTrace()
    case Right(i) ⇒ println(s"result is $i")
  }

}
