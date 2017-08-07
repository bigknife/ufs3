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
import block.Block.BlockFile
import cats.free.Inject
import sop._

trait Filler[F[_]] {
  import Filler.FillerFile

  def init(bf: BlockFile): RespPar[F, FillerFile]
  def check(bf: BlockFile): RespPar[F, FillerFile]
  def close(ff: FillerFile): RespPar[F, Unit]
  // allocate space for new sandwich, return the startpoint
  def startAppend(ff: FillerFile): RespPar[F, Long]
  def endAppend(ff: FillerFile, startPosition: Long, endPosition: Long): RespPar[F, FillerFile]
  def freeSpace(ff: FillerFile): RespPar[F, Long]
  def isWriting(ff: FillerFile): RespPar[F, Boolean]
  def forceToWrite(ff: FillerFile): RespPar[F, Unit]
}
object Filler {
  sealed trait Op[A]
  final case class Init(bf: BlockFile)                                               extends Op[Resp[FillerFile]]
  final case class Check(bf: BlockFile)                                              extends Op[Resp[FillerFile]]
  final case class Close(ff: FillerFile)                                             extends Op[Resp[Unit]]
  final case class StartAppend(ff: FillerFile)                                       extends Op[Resp[Long]]
  final case class EndAppend(ff: FillerFile, startPosition: Long, endPosition: Long) extends Op[Resp[FillerFile]]
  final case class FreeSpace(ff: FillerFile)                                         extends Op[Resp[Long]]
  final case class IsWriting(ff: FillerFile)                                         extends Op[Resp[Boolean]]
  final case class ForceToWrite(ff: FillerFile)                                      extends Op[Resp[Unit]]

  class To[F[_]](implicit I: Inject[Op, F]) extends Filler[F] {
    def init(bf: BlockFile): RespPar[F, FillerFile]   = liftPar_T[Op, F, Resp[FillerFile]](Init(bf))
    def check(bf: BlockFile): RespPar[F, FillerFile]  = liftPar_T[Op, F, Resp[FillerFile]](Check(bf))
    def close(ff: FillerFile): RespPar[F, Unit]       = liftPar_T[Op, F, Resp[Unit]](Close(ff))
    def startAppend(ff: FillerFile): RespPar[F, Long] = liftPar_T[Op, F, Resp[Long]](StartAppend(ff))

    def endAppend(ff: FillerFile, startPosition: Long, endPosition: Long): RespPar[F, FillerFile] =
      liftPar_T[Op, F, Resp[FillerFile]](EndAppend(ff, startPosition, endPosition))

    def freeSpace(ff: FillerFile): RespPar[F, Long]    = liftPar_T[Op, F, Resp[Long]](FreeSpace(ff))
    def isWriting(ff: FillerFile): RespPar[F, Boolean] = liftPar_T[Op, F, Resp[Boolean]](IsWriting(ff))
    def forceToWrite(ff: FillerFile): RespPar[F, Unit] = liftPar_T[Op, F, Resp[Unit]](ForceToWrite(ff))
  }

  implicit def to[F[_]](implicit I: Inject[Op, F]): Filler[F] = new To[F]

  def apply[F[_]](implicit F: Filler[F]): Filler[F] = F

  trait Handler[M[_]] extends NT[Op, M] {
    def init(blockFile: BlockFile): M[Resp[FillerFile]]
    def check(blockFile: BlockFile): M[Resp[FillerFile]]
    def close(ff: FillerFile): M[Resp[Unit]]
    def startAppend(ff: FillerFile): M[Resp[Long]]
    def endAppend(ff: FillerFile, startPosition: Long, endPosition: Long): M[Resp[FillerFile]]
    def freeSpace(ff: FillerFile): M[Resp[Long]]
    def isWriting(ff: FillerFile): M[Resp[Boolean]]
    def forceToWrite(ff: FillerFile): M[Resp[Unit]]

    override def apply[A](fa: Op[A]): M[A] = fa match {
      case Init(bf)                                  ⇒ init(bf)
      case Check(bf)                                 ⇒ check(bf)
      case Close(ff)                                 ⇒ close(ff)
      case StartAppend(ff)                           ⇒ startAppend(ff)
      case EndAppend(ff, startPosition, endPosition) ⇒ endAppend(ff, startPosition, endPosition)
      case FreeSpace(ff)                             ⇒ freeSpace(ff)
      case IsWriting(ff)                             ⇒ isWriting(ff)
      case ForceToWrite(ff)                          ⇒ forceToWrite(ff)
    }
  }

  private[ufs3] trait FillerFile
}
