/**
  * filler.scala
  * ----------
  * The ufs3 store's alias. The ufs3's store is just like some blank host list, and save
  * file is just like filling blank. That's the name is from.
  * @author bigknife
  * @since 2017/7/6
  */
package ufs3
package kernel
package filler

import scala.language.higherKinds
import scala.language.implicitConversions
import block._
import Block._
import cats.free.{Free, Inject}
import Free._

sealed trait Filler[A]
object Filler {
  final case class InitBlock(bf: BlockFile)     extends Filler[Response[Unit]]
  final case class ValidateBlock(bf: BlockFile) extends Filler[Response[Unit]]

  final class Ops[F[_]](implicit I: Inject[Filler, F]) {
    def initBlock(bf: BlockFile): Free[F, Response[Unit]]     = inject[Filler, F](InitBlock(bf))
    def validateBlock(bf: BlockFile): Free[F, Response[Unit]] = inject[Filler, F](ValidateBlock(bf))

    def openBlock(path: Path, mode: FileMode, size: Size)(implicit B: Block.Ops[F]): Free[F, Response[Unit]] =
      for {
        rof ← B.open(path, mode)
        a ← rof match {
          case Left(t)        ⇒ freeError[F, Unit](t)
          case Right(Some(x)) ⇒ validateBlock(bf = x) // 如果已经有文件了，直接进行验证
          case Right(None)    ⇒
            // 如果没有，则新建，然后进行初始化
            for {
              rof1 ← B.create(path, size)
              a1 ← rof1 match {
                case Left(t)   ⇒ freeError[F, Unit](t)
                case Right(bf) ⇒ initBlock(bf)
              }
            } yield a1
        }
      } yield a
  }

  object Ops {
    implicit def ops[F[_]](implicit I: Inject[Filler, F]) = new Ops[F]
  }
}

sealed trait FillerFile
