package infr
package util
package interpreter

import cats.Eval
import cats.data.Kleisli
import cats.effect.IO

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.concurrent.ExecutionContext.Implicits.global


/**
  * Created by songwenchao on 2017/7/18.
  */
object FutureConverter {
  implicit def liftFToK[A, C](future: Future[A]): Kleisli[IO, C, A] = Kleisli { _: C ⇒
    IO.fromFuture[A] {
      Eval.later(future)
    }
  }
}
