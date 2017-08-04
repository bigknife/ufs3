/**
  * sop_test2.scala
  * ----------
  *
  * @author bigknife
  * @since 2017/8/4
  */
package sop
package test2

import java.util.concurrent.atomic.AtomicLong

import cats.Id
import cats.free.Inject
import sop._
import sop.test2.SomeAction.UnsafeDo

import scala.language.higherKinds

trait SomeAction[F[_]] {
  def unsafeDo(): Par[F, Either[Throwable, Unit]]
}

object SomeAction {
  sealed trait Op[A]
  final case class UnsafeDo() extends Op[Either[Throwable, Unit]]

  class To[F[_]](implicit I: Inject[Op, F]) extends SomeAction[F] {
    def unsafeDo(): Par[F, Either[Throwable, Unit]] = liftPar_T[Op, F, Either[Throwable, Unit]](UnsafeDo())
  }
  implicit def to[F[_]](implicit I: Inject[Op, F]): SomeAction[F] = new To[F]

  def apply[F[_]](implicit S: SomeAction[F]) = S
}

object Test extends App {
  val action = SomeAction[SomeAction.Op]

  val prog: SOP[SomeAction.Op, Either[Throwable, Unit]] = for {
    res1 ← action.unsafeDo()
    res2 ← res1 match {
      case x@Left(t) ⇒ SOP.pure[SomeAction.Op, Either[Throwable, Unit]](x)
      case Right(_) ⇒ action.unsafeDo(): SOP[SomeAction.Op, Either[Throwable, Unit]]
    }
  } yield res2

  val interpreter = new NT[SomeAction.Op, Id] {
    val count = new AtomicLong(0)
    def apply[A](fa: SomeAction.Op[A]): Id[A] = fa match {
      case UnsafeDo() ⇒
        val c = count.getAndIncrement()
        if (c % 2 == 0) Right(println(s"unsafe do $c"))
        else Left(new Exception(s"counter is $c"))
    }
  }

  prog.foldMap(interpreter)
}
