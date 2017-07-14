/**
  * package object sop
  * ----------
  * Sequencial Computation Over Paralle Computation
  * @author bigknife
  * @since 2017/7/12
  */


import cats.free.{Free, FreeApplicative, Inject}
import cats.{Applicative, ~>}

import scala.language.higherKinds
import scala.language.implicitConversions

package object sop {
  // parallel computaion using FreeApplicative
  type Par[F[_], A] = FreeApplicative[F, A]
  // sequencial computaion using Free
  type Seq[F[_], A] = Free[F, A]
  // seq over par
  type SOP[F[_], A] = Seq[Par[F, ?], A]

  // natural transform
  type NT[F[_], G[_]] = (F ~> G)

  // Par companion boject
  object Par {
    def pure[F[_], A](a: A): Par[F, A] = FreeApplicative.pure[F, A](a)
  }

  // lift F[A] to Par[F, A]
  def liftPar[F[_], A](fa: F[A]): Par[F, A] = FreeApplicative.lift(fa)
  // lift F[A] to Par[G, A] using a implicit Inject[F, G] instance
  def liftPar_T[F[_], G[_], A](fa: F[A])(implicit I: Inject[F, G]): Par[G, A] = {
    val par = liftPar(fa)
    par.compile(new NT[F, G] {
      override def apply[B](fa: F[B]): G[B] = I.inj(fa)
    })
  }

  // lift F[A] to SOP[F, A]
  def liftSOP[F[_], A](fa: F[A]): SOP[F, A] = {
    val par = liftPar(fa)
    Free.liftF[Par[F, ?], A](par)
  }
  // lift F[A] to SOP[G, A] using a implicit Inject[F, G] instance
  def liftSOP_T[F[_], G[_], A](fa: F[A])(implicit I: Inject[F, G]): SOP[G, A] = {
    val sopF = liftSOP(fa)
    sopF.compile[Par[G, ?]](new NT[Par[F, ?], Par[G, ?]] {
      override def apply[B](fa: Par[F, B]): Par[G, B] =
        fa.compile(new NT[F, G] {
          override def apply[C](fa: F[C]): G[C] = I.inj(fa)
        })
    })
  }

  // lift Par[F, A] to SOP[F, A]
  // with `implicit`, we can use `for` to composite Par[F, A]
  // cause the program of Par[F, A] cant be transformed to SOP[F, A] implicitly
  implicit def liftPAR_to_SOP[F[_], A](par: Par[F, A]): SOP[F, A] = {
    Free.liftF[Par[F, ?], A](par)
  }
  // lift NT[F, G] to NT[Par[F], G]
  // with `implicit`, the program of SOP[F, A] can be interpreted by NT[F, G]
  // cause it be transformed to NT[Par[F], G] implicitly
  implicit def liftInterpreter[F[_], G[_]: Applicative](nt: NT[F, G]): NT[Par[F, ?], G] = new NT[Par[F, ?], G] {
    override def apply[A](fa: Par[F, A]): G[A] = fa.foldMap(nt)
  }
}
