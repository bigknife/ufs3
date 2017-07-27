/**
  * sandwich.scala
  * ----------
  * Sandwich ADT, Sandwich is the small file layout in the big file(FillerFile).
  * It contains `Head` `Body` `Tail`
  * @author bigknife
  * @since 2017/7/14
  */
package ufs3
package kernel
package sandwich

import java.nio.ByteBuffer

import cats.free.Inject

import scala.language.higherKinds
import scala.language.implicitConversions
import sop._

trait SandwichIn[F[_], In] {
  def head(key: String, uuid: String, bodyLength: Long): Par[F, ByteBuffer]
  def nextBody(in: In): Par[F, Option[ByteBuffer]]
  def tail(hash: Array[Byte], bodyLength: Long): Par[F, ByteBuffer]
}
object SandwichIn {
  trait Op[In, A]
  final case class Head[In](key: String, uuid: String, bodyLength: Long) extends Op[In, ByteBuffer]
  final case class NextBody[In](in: In)                                  extends Op[In, Option[ByteBuffer]]
  final case class Tail[In](hash: Array[Byte], bodyLength: Long)         extends Op[In, ByteBuffer]

  class To[F[_], In](implicit IJ: Inject[Op[In, ?], F]) extends SandwichIn[F, In] {
    def nextBody(in: In): Par[F, Option[ByteBuffer]] = liftPar_T[Op[In, ?], F, Option[ByteBuffer]](NextBody[In](in))
    def head(key: String, uuid: String, bodyLength: Long): Par[F, ByteBuffer] =
      liftPar_T[Op[In, ?], F, ByteBuffer](Head[In](key, uuid, bodyLength))
    def tail(hash: Array[Byte], bodyLength: Long): Par[F, ByteBuffer] =
      liftPar_T[Op[In, ?], F, ByteBuffer](Tail[In](hash, bodyLength))
  }

  implicit def to[F[_], In](implicit IJ: Inject[Op[In, ?], F]): SandwichIn[F, In] = new To[F, In]

  def apply[F[_], In](implicit S: SandwichIn[F, In]): SandwichIn[F, In] = S

  trait Handler[M[_], In] extends NT[Op[In, ?], M] {
    def head(key: String, uuid: String, bodyLength: Long): M[ByteBuffer]
    def nextBody(in: In): M[Option[ByteBuffer]]
    def tail(hash: Array[Byte], bodyLength: Long): M[ByteBuffer]

    def apply[A](fa: Op[In, A]): M[A] = fa match {
      case Head(key, uuid, bodyLength) ⇒ head(key, uuid, bodyLength)
      case NextBody(in)                ⇒ nextBody(in)
      case Tail(hash, bodyLength)      ⇒ tail(hash, bodyLength)
    }
  }
}

trait SandwichOut[F[_], Out] {
  def headSize(): Par[F, Long]
  def tailSize(): Par[F, Long]
  def head(bb: ByteBuffer, out: Out): Par[F, Unit]
  def outputBody(body: ByteBuffer, out: Out): Par[F, Unit]
  def tail(bb: ByteBuffer, out: Out): Par[F, Unit]
}
object SandwichOut {
  sealed trait Op[Out, A]
  final case class HeadSize[Out]()                             extends Op[Out, Long]
  final case class TailSize[Out]()                             extends Op[Out, Long]
  final case class Head[Out](bb: ByteBuffer, out: Out)         extends Op[Out, Unit]
  final case class OutputBody[Out](body: ByteBuffer, out: Out) extends Op[Out, Unit]
  final case class Tail[Out](bb: ByteBuffer, out: Out)         extends Op[Out, Unit]

  class To[F[_], Out](implicit I: Inject[Op[Out, ?], F]) extends SandwichOut[F, Out] {
    def head(bb: ByteBuffer, out: Out): Par[F, Unit]         = liftPar_T[Op[Out, ?], F, Unit](Head(bb, out))
    def outputBody(body: ByteBuffer, out: Out): Par[F, Unit] = liftPar_T[Op[Out, ?], F, Unit](OutputBody(body, out))
    def tail(bb: ByteBuffer, out: Out): Par[F, Unit]         = liftPar_T[Op[Out, ?], F, Unit](Tail(bb, out))
    def headSize(): Par[F, Long]                             = liftPar_T[Op[Out, ?], F, Long](HeadSize())
    override def tailSize(): Par[F, Long]                    = liftPar_T[Op[Out, ?], F, Long](TailSize())

  }

  implicit def to[F[_], Out](implicit I: Inject[Op[Out, ?], F]): SandwichOut[F, Out] = new To[F, Out]

  def apply[F[_], Out](implicit S: SandwichOut[F, Out]): SandwichOut[F, Out] = S

  trait Handler[M[_], Out] extends NT[Op[Out, ?], M] {
    def head(bb: ByteBuffer, out: Out): M[Unit]
    def outputBody(body: ByteBuffer, out: Out): M[Unit]
    def tail(bb: ByteBuffer, out: Out): M[Unit]
    def headSize(): M[Long]
    def tailSize(): M[Long]

    def apply[A](fa: Op[Out, A]): M[A] = fa match {
      case Head(bb, out)         ⇒ head(bb, out)
      case OutputBody(body, out) ⇒ outputBody(body, out)
      case Tail(bb, out)         ⇒ tail(bb, out)
      case HeadSize()            ⇒ headSize()
      case TailSize()            ⇒ tailSize()
    }
  }
}
