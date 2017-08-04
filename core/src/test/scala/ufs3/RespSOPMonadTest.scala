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

import cats.Id
import cats.free.Inject
import sop._
import ufs3.core.monad.SomeAction.UnsafeDo

import scala.language.higherKinds
import ufs3.core.data.Data._

trait SomeAction[F[_]] {
  def unsafeDo(): Par[F, Either[Throwable, Int]]
}

object SomeAction {
  sealed trait Op[A]
  final case class UnsafeDo() extends Op[Either[Throwable, Int]]

  class To[F[_]](implicit I: Inject[Op, F]) extends SomeAction[F] {
    def unsafeDo(): Par[F, Either[Throwable, Int]] = liftPar_T[Op, F, Either[Throwable, Int]](UnsafeDo())
  }
  implicit def to[F[_]](implicit I: Inject[Op, F]): SomeAction[F] = new To[F]

  def apply[F[_]](implicit S: SomeAction[F]) = S
}

object Test extends App {
  val action = SomeAction[SomeAction.Op]

  val prog: RespSOP[SomeAction.Op, Int] = for {
    res1 ← action.unsafeDo(): RespSOP[SomeAction.Op, Int]
    res2 ← action.unsafeDo(): RespSOP[SomeAction.Op, Int]
    res3 ← action.unsafeDo(): RespSOP[SomeAction.Op, Int]
  } yield res3

  val interpreter = new NT[SomeAction.Op, Id] {
    val count = new AtomicLong(0)
    def apply[A](fa: SomeAction.Op[A]): Id[A] = fa match {
      case UnsafeDo() ⇒
        val c = count.getAndIncrement()
        if (c % 3 != 0) Right(c.toInt)
        else Left(new Exception(s"counter is $c"))
    }
  }

  prog.foldMap(interpreter) match {
    case Left(t) ⇒ t.printStackTrace()
    case Right(i) ⇒ println(s"result is $i")
  }
}
