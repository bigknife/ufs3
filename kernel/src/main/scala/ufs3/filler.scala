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
import cats.free.Inject
import sop._

trait Filler[F[_]] {
  def init(bf: BlockFile): Par[F, FillerFile]
  def validate(bf: BlockFile): Par[F, FillerFile]
  def checkIndex(ff: FillerFile): Par[F, Boolean]
  def repairIndex(ff: FillerFile): Par[F, Unit]
}
object Filler {
  sealed trait Op[A]
  final case class Init(bf: BlockFile)         extends Op[FillerFile]
  final case class Validate(bf: BlockFile)     extends Op[FillerFile]
  final case class CheckIndex(ff: FillerFile)  extends Op[Boolean]
  final case class RepairIndex(ff: FillerFile) extends Op[Unit]

  class To[F[_]](implicit I: Inject[Op, F]) extends Filler[F] {
    def init(bf: BlockFile): Par[F, FillerFile]     = liftPar_T[Op, F, FillerFile](Init(bf))
    def validate(bf: BlockFile): Par[F, FillerFile] = liftPar_T[Op, F, FillerFile](Validate(bf))
    def checkIndex(ff: FillerFile): Par[F, Boolean] = liftPar_T[Op, F, Boolean](CheckIndex(ff))
    def repairIndex(ff: FillerFile): Par[F, Unit]   = liftPar_T[Op, F, Unit](RepairIndex(ff))
  }

  implicit def to[F[_]](implicit I: Inject[Op, F]): Filler[F] = new To[F]

  def apply[F[_]](implicit F: Filler[F]): Filler[F] = F

  trait Handler[M[_]] extends NT[Op, M] {
    def init(blockFile: BlockFile): M[FillerFile]
    def validate(blockFile: BlockFile): M[FillerFile]
    def checkIndex(ff: FillerFile): M[Boolean]
    def repairIndex(ff: FillerFile): M[Unit]

    override def apply[A](fa: Op[A]): M[A] = fa match {
      case Init(bf)        => init(bf)
      case Validate(bf)    ⇒ validate(bf)
      case CheckIndex(ff)  ⇒ checkIndex(ff)
      case RepairIndex(ff) ⇒ repairIndex(ff)
    }
  }

}

sealed trait FillerFile
object FillerFile {
  def apply(): FillerFile = new FillerFile() {}
}
