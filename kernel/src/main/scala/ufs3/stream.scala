/**
  * stream.scala
  * ------------
  * the kernel adt of stream
  * @author: bigknife@outlook.com
  * @create: 2017/07/03
  */

package ufs3
package kernel
package stream

import scala.language.higherKinds
import scala.language.implicitConversions

import cats.free.{Free, Inject}
import Free._
import java.nio.ByteBuffer

/**
  * Stream adt
  */
sealed trait Stream[A]
object Stream {
  case object Next extends Stream[Option[Data]]

  class Ops[F[_]](implicit I: Inject[Stream, F]) {
    def next: Free[F, Option[Data]] = inject[Stream, F](Next)
  }
  implicit def ops[F[_]](implicit I: Inject[Stream, F]) = new Ops[F]
}

/**
  * stream data
  */
sealed trait Data {
  def byteBuffer: ByteBuffer
}
