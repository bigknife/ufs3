/**
  * interpreter.scala
  * ----------
  * the lifted interperter for UniConfig
  * @author bigknife
  * @since 2017/7/21
  */
package ufs3
package integration

import java.io.{InputStream, OutputStream}

import cats.data.Kleisli
import cats.effect.IO
import ufs3.interpreter.block.BlockInterpreter
import ufs3.interpreter.fildex.FildexInterpreter
import ufs3.interpreter.filler.FillerInterpreter
import ufs3.interpreter.sandwich.{SandwichInInterpreter, SandwichOutInterpreter}
import ufs3.log.interpreter.LogInterpreter
import sop._
import ufs3.integration.config.UniConfig
import ufs3.kernel.block.Block._
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import ufs3.kernel.sandwich.{SandwichIn, SandwichOut}

import scala.language.higherKinds

package object interpreter {
  val bi  = BlockInterpreter()
  val fi  = FillerInterpreter()
  val fii = FildexInterpreter()
  val li  = LogInterpreter()
  val sii = SandwichInInterpreter()
  val soi = SandwichOutInterpreter()

  // NT → NT
  def liftNT[F[_], G[_], H[_]](nt: NT[F, G])(implicit nt1: NT[G, H]): NT[F, H] = nt1 compose nt

  implicit val nt1 = new NT[Kleisli[IO, BlockInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]] {
    def apply[A](fa: Kleisli[IO, BlockInterpreter.Config, A]): Kleisli[IO, UniConfig, A] =
      fa.local[UniConfig](_.blockConfig)
  }
  implicit val nt2 = new NT[Kleisli[IO, FillerInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]] {
    def apply[A](fa: Kleisli[IO, FillerInterpreter.Config, A]): Kleisli[IO, UniConfig, A] =
      fa.local[UniConfig](_.fillerConfig)
  }
  implicit val nt3 = new NT[Kleisli[IO, FildexInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]] {
    def apply[A](fa: Kleisli[IO, FildexInterpreter.Config, A]): Kleisli[IO, UniConfig, A] =
      fa.local[UniConfig](_.fildexConfig)
  }
  implicit val nt4 = new NT[Kleisli[IO, LogInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]] {
    def apply[A](fa: Kleisli[IO, LogInterpreter.Config, A]): Kleisli[IO, UniConfig, A] =
      fa.local[UniConfig](_.logConfig)
  }
  implicit val nt5 = new NT[Kleisli[IO, SandwichInInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]] {
    def apply[A](fa: Kleisli[IO, SandwichInInterpreter.Config, A]): Kleisli[IO, UniConfig, A] =
      fa.local[UniConfig](_.sandwichInConfig)
  }
  implicit val nt6 = new NT[Kleisli[IO, SandwichOutInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]] {
    def apply[A](fa: Kleisli[IO, SandwichOutInterpreter.Config, A]): Kleisli[IO, UniConfig, A] =
      fa.local[UniConfig](_.sanwichOutConfig)
  }

  val blockInterperter: NT[Op, Kleisli[IO, UniConfig, ?]] =
    liftNT[Op, Kleisli[IO, BlockInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]](bi)
  val fillerInterperter: NT[Filler.Op, Kleisli[IO, UniConfig, ?]] =
    liftNT[Filler.Op, Kleisli[IO, FillerInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]](fi)
  val fildexInterperter: NT[Fildex.Op, Kleisli[IO, UniConfig, ?]] =
    liftNT[Fildex.Op, Kleisli[IO, FildexInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]](fii)
  val logInterperter: NT[Log.Op, Kleisli[IO, UniConfig, ?]] =
    liftNT[Log.Op, Kleisli[IO, LogInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]](li)
  val sandwichInInterpreter: NT[SandwichIn.Op[InputStream, ?], Kleisli[IO, UniConfig, ?]] =
    liftNT[SandwichIn.Op[InputStream, ?], Kleisli[IO, SandwichInInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]](sii)
  val sandwichOutInterpreter: NT[SandwichOut.Op[OutputStream, ?], Kleisli[IO, UniConfig, ?]] =
    liftNT[SandwichOut.Op[OutputStream, ?], Kleisli[IO, SandwichOutInterpreter.Config, ?], Kleisli[IO, UniConfig, ?]](
      soi)

}