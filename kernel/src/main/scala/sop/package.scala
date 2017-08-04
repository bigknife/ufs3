/**
  * package object sop
  * ----------
  * Sequencial Computation Over Paralle Computation
  * @author bigknife
  * @since 2017/7/12
  */


import cats.free.{Free, FreeApplicative, Inject}
import cats.{Applicative, Monad, ~>}

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
  object SOP {
    def pure[F[_], A](a: A): SOP[F, A] = liftPAR_to_SOP(Par.pure[F, A](a))
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


  // common response for operation
  type Resp[A] = Either[Throwable, A]
  object Resp {
    def pure[A](a: A): Resp[A] = Right(a)
    def right[A](a: ⇒ A): Resp[A] = pure(a)
    def error[A](t: Throwable): Resp[A] = Left(t)
  }

  type RespPar[F[_], A] = Par[F, Resp[A]]

  type RespSOP[F[_], A] = SOP[F, Resp[A]]
  object RespSOP {
    def pure[F[_], A](x: A): RespSOP[F, A] = {
      SOP.pure[F, Resp[A]](Resp.pure(x))
    }
    def error[F[_], A](t: Throwable): RespSOP[F, A] = {
      SOP.pure[F, Resp[A]](Resp.error(t))
    }
    implicit class OpForSOP[F[_], A](respSOP: RespSOP[F, A]) {
      def asM: Monad.AllOps[RespSOP[F, ?], A] = toRespSOPMonad(respSOP)
    }
    implicit class OpForPar[F[_], A](par: Par[F, Resp[A]]) {
      def asM: Monad.AllOps[RespSOP[F, ?], A] = toRespSOPMonad(par: RespSOP[F, A])
    }
  }

  implicit def RespSOPMonad[F[_]] = new Monad[RespSOP[F, ?]] {
    def pure[A](x: A): RespSOP[F, A] = RespSOP.pure[F, A](x)

    def flatMap[A, B](fa: RespSOP[F, A])(f: (A) => RespSOP[F, B]): RespSOP[F, B] = {
      for {
        respa ← fa
        x ← respa match {
          case Left(t) ⇒ RespSOP.error[F, B](t)
          case Right(a) ⇒ f(a)
        }
      } yield x
    }

    def tailRecM[A, B](a: A)(f: (A) => RespSOP[F, Either[A, B]]): RespSOP[F, B] = {
      for {
        fe ← f(a)
        x ← (fe: Resp[Either[A, B]]) match {
          case Left(t) ⇒ RespSOP.error[F, B](t)
          case Right(Left(xa)) ⇒ tailRecM[A, B](xa)(f)
          case Right(Right(xb)) ⇒ pure[B](xb)
        }
      } yield x
    }
  }

  implicit def toRespSOPMonad[F[_], A](x: RespSOP[F, A]): Monad.AllOps[RespSOP[F, ?], A] = Monad.ops.toAllMonadOps(x)
}
