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

trait Sandwich[F[_], IN] {
  def head(): Par[F, ByteBuffer]
  def nextBody(in: IN): Par[F, Option[ByteBuffer]]
  def tail(): Par[F, ByteBuffer]
}

object Sandwich {
  trait Op[IN, A]
  final case class Head[IN]()    extends Op[IN, ByteBuffer]
  final case class NextBody[IN](in: IN) extends Op[IN, Option[ByteBuffer]]
  final case class Tail[IN]()    extends Op[IN, ByteBuffer]

  class To[F[_], IN](implicit IJ: Inject[Op[IN, ?], F]) extends Sandwich[F, IN] {
    def nextBody(in: IN): Par[F, Option[ByteBuffer]] = liftPar_T[Op[IN, ?], F, Option[ByteBuffer]](NextBody[IN](in))
    def head(): Par[F, ByteBuffer]             = liftPar_T[Op[IN, ?], F, ByteBuffer](Head[IN]())
    def tail(): Par[F, ByteBuffer]             = liftPar_T[Op[IN, ?], F, ByteBuffer](Tail[IN]())
  }

  implicit def to[F[_], IN](implicit IJ: Inject[Op[IN, ?], F]): Sandwich[F, IN] = new To[F, IN]

  def apply[F[_], IN](implicit S: Sandwich[F, IN]): Sandwich[F, IN] = S

  trait Handler[M[_], IN] extends NT[Op[IN, ?], M] {
    def head(): M[ByteBuffer]
    def nextBody(in: IN): M[Option[ByteBuffer]]
    def tail(): M[ByteBuffer]

    def apply[A](fa: Op[IN, A]): M[A] = fa match {
      case Head() ⇒ head()
      case NextBody(in) ⇒ nextBody(in)
      case Tail() ⇒ tail()
    }
  }
}