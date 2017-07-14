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
import block.Block._
import cats.free.Inject
import sop._

trait Filler[F[_]] {
  import Filler.FillerFile

  def init(bf: BlockFile): Par[F, FillerFile]
  def check(bf: BlockFile): Par[F, FillerFile]
}
object Filler {
  sealed trait Op[A]
  final case class Init(bf: BlockFile)  extends Op[FillerFile]
  final case class Check(bf: BlockFile) extends Op[FillerFile]

  class To[F[_]](implicit I: Inject[Op, F]) extends Filler[F] {
    def init(bf: BlockFile): Par[F, FillerFile]  = liftPar_T[Op, F, FillerFile](Init(bf))
    def check(bf: BlockFile): Par[F, FillerFile] = liftPar_T[Op, F, FillerFile](Check(bf))
  }

  implicit def to[F[_]](implicit I: Inject[Op, F]): Filler[F] = new To[F]

  def apply[F[_]](implicit F: Filler[F]): Filler[F] = F

  trait Handler[M[_]] extends NT[Op, M] {
    def init(blockFile: BlockFile): M[FillerFile]
    def check(blockFile: BlockFile): M[FillerFile]

    override def apply[A](fa: Op[A]): M[A] = fa match {
      case Init(bf)  => init(bf)
      case Check(bf) ⇒ check(bf)
    }
  }

  sealed trait FillerFile
  object FillerFile {
    def apply(): FillerFile = new FillerFile() {}
  }
}

