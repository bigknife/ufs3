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

import ufs3.kernel.stream.Data.DataTransformer

/**
  * Stream adt
  */
sealed trait Stream[A]
object Stream {
  case object Next extends Stream[Option[Data]]

  class Ops[F[_]](implicit I: Inject[Stream, F]) {
    def next: Free[F, Option[Data]] = inject[Stream, F](Next)
  }

  object Ops {
    implicit def ops[F[_]](implicit I: Inject[Stream, F]) = new Ops[F]

    //todo: add a transformer to Store.Data
    implicit val storeDataTransformer = new DataTransformer[ufs3.kernel.store.Data](sd ⇒ ???)
  }
}

/**
  * stream data
  */
sealed trait Data {self ⇒
  import Data._
  def byteBuffer: ByteBuffer
  def to[A](implicit transformer: DataTransformer[A]): A = transformer.run(self)
}

object Data {
  final case class DataTransformer[A](run: Data ⇒ A)

}
