/**
  * fildex.scala
  * ----------
  * Filler File Index
  * @author bigknife
  * @since 2017/7/13
  */
package ufs3
package kernel
package fildex

import cats.free.Inject
import filler.Filler.FillerFile

import scala.language.higherKinds
import scala.language.implicitConversions
import sop._

trait Fildex[F[_]] {
  import Fildex.FildexFile

  def check(ff: FillerFile): Par[F, Boolean]
  def repair(ff: FillerFile): Par[F, Unit]
  def create(ff: FillerFile): Par[F, FildexFile]
  def append(ff: FildexFile, key: String, startPos: Long, endPos: Long): Par[F, Unit]
}

object Fildex {
  sealed trait Op[A]
  final case class Check(ff: FillerFile)                                             extends Op[Boolean]
  final case class Repair(ff: FillerFile)                                            extends Op[Unit]
  final case class Create(ff: FillerFile)                                            extends Op[FildexFile]
  final case class Append(ff: FildexFile, key: String, startPos: Long, endPos: Long) extends Op[Unit]

  class To[F[_]](implicit I: Inject[Op, F]) extends Fildex[F] {
    def check(ff: FillerFile): Par[F, Boolean]     = liftPar_T[Op, F, Boolean](Check(ff))
    def repair(ff: FillerFile): Par[F, Unit]       = liftPar_T[Op, F, Unit](Repair(ff))
    def create(ff: FillerFile): Par[F, FildexFile] = liftPar_T[Op, F, FildexFile](Create(ff))
    def append(ff: FildexFile, key: String, startPos: Long, endPos: Long): Par[F, Unit] =
      liftPar_T[Op, F, Unit](Append(ff, key, startPos, endPos))
  }
  implicit def to[F[_]](implicit I: Inject[Op, F]): Fildex[F] = new To[F]

  def apply[F[_]](implicit F: Fildex[F])                      = F

  trait Handler[M[_]] extends NT[Op, M] {

    def check(ff: FillerFile): M[Boolean]
    def repair(ff: FillerFile): M[Unit]
    def create(ff: FillerFile): M[FildexFile]
    def append(ff: FildexFile, key: String, startPos: Long, endPos: Long): M[Unit]

    def apply[A](fa: Op[A]): M[A] = fa match {
      case Check(ff)                         ⇒ check(ff)
      case Repair(ff)                        ⇒ repair(ff)
      case Create(ff)                        ⇒ create(ff)
      case Append(ff, key, startPos, endPos) ⇒ append(ff, key, startPos, endPos)
    }
  }

  trait FildexFile
  trait Data
}

