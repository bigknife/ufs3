/**
  * core_data.scala
  * ----------
  * data structure
  * @author bigknife
  * @since 2017/7/27
  */

package ufs3
package core
package data

import java.util.concurrent.atomic.AtomicReference

import cats.Monad
import ufs3.kernel.block.Block
import ufs3.kernel.block.Block.BlockFile
import ufs3.kernel.fildex.Fildex.FildexFile
import ufs3.kernel.filler.Filler.FillerFile
import sop._

import scala.language.higherKinds

trait Data {
  val EmptyUUID: String = "                                " //32 space string
  // UFS3 trait
  trait UFS3 {
    def blockFile: AtomicReference[BlockFile]
    def fillerFile: AtomicReference[FillerFile]
    def fildexFile: AtomicReference[FildexFile]
  }

  object UFS3 {
    private[core] def apply(_blockFile: BlockFile, _fillerFile: FillerFile, _fildexFile: FildexFile): UFS3 = new UFS3 {
      val fildexFile: AtomicReference[FildexFile] = new AtomicReference[FildexFile](_fildexFile)
      val fillerFile: AtomicReference[FillerFile] = new AtomicReference[FillerFile](_fillerFile)
      val blockFile: AtomicReference[BlockFile]   = new AtomicReference[BlockFile](_blockFile)
    }
  }

  // core config
  trait CoreConfig {
    def fillerBlockPath: Block.Path
    def fillerBlockSize: Block.Size
    def idxBlockSize: Block.Size
    def fillerReadBufferSize: Block.Size
  }

  type Resp[A] = Either[Throwable, A]
  object Resp {
    def pure[A](a: A): Resp[A] = Right(a)
    def right[A](a: ⇒ A): Resp[A] = pure(a)
    def error[A](t: Throwable): Resp[A] = Left(t)
  }

  type RespSOP[F[_], A] = SOP[F, Resp[A]]
  object RespSOP {
    def pure[F[_], A](x: A): RespSOP[F, A] = {
      SOP.pure[F, Resp[A]](Resp.pure(x))
    }
    def error[F[_], A](t: Throwable): RespSOP[F, A] = {
      SOP.pure[F, Resp[A]](Resp.error(t))
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
}
object Data extends Data