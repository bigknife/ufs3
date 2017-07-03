/**
  * store.scala
  * -----------
  * the kernel adt of store
  * @author: bigknife@outlook.com
  * @create: 2017/07/03
  */

package ufs3
package kernel
package store

import scala.language.higherKinds
import scala.language.implicitConversions
import cats.free.{Free, Inject}
import Free._

/**
  * Store Free Monad
  */
sealed trait Store[A]
object Store {
  type Response[A] = Either[Throwable, A]

  case class Existed(path: Path) extends Store[Response[Boolean]]
  case class IsLegal(path: Path) extends Store[Response[Boolean]]

  case class Create(path: Path, size: Size) extends Store[Response[Filler]]
  case class Delete(path: Path) extends Store[Response[Unit]]

  case class Open(path: Path, mode: FileMode) extends Store[Response[Filler]]
  case class Close(filler: Filler) extends Store[Response[Unit]]

  case class Read(filler: Filler, size: Size) extends Store[Response[Data]]
  case class Write(filler: Filler, data: Data) extends Store[Response[Unit]]

  case class Lock(filler: Filler) extends Store[Response[Unit]]
  case class UnLock(filler: Filler) extends Store[Response[Unit]]

  case class FreeSpace(filler: Filler) extends Store[Response[Size]]
  case class IsWriting(filler: Filler) extends Store[Response[Boolean]]

  class Ops[F[_]](implicit I: Inject[Store, F]){
    import Free._

    def existed(path: Path): Free[F, Response[Boolean]] = inject[Store, F](Existed(path))
    def isLegal(path: Path): Free[F, Response[Boolean]] = inject[Store, F](IsLegal(path))

    def create(path: Path, size: Size): Free[F, Response[Filler]] = inject[Store, F](Create(path, size))
    def delete(path: Path): Free[F, Response[Unit]] = inject[Store, F](Delete(path))

    def open(path: Path, mode: FileMode): Free[F, Response[Filler]] = inject[Store, F](Open(path, mode))
    def close(filler: Filler): Free[F, Response[Unit]] = inject[Store, F](Close(filler))

    def read(filler: Filler, size: Size): Free[F, Response[Data]] = inject[Store, F](Read(filler, size))
    def write(filler: Filler, data: Data): Free[F, Response[Unit]] = inject[Store, F](Write(filler, data))

    def lock(filler: Filler): Free[F, Response[Unit]] = inject[Store, F](Lock(filler))
    def unlock(filler: Filler): Free[F, Response[Unit]] = inject[Store, F](UnLock(filler))

    def freeSpace(filler: Filler): Free[F, Response[Size]] = inject[Store, F](FreeSpace(filler))
    def isWriting(filler: Filler): Free[F, Response[Boolean]] = inject[Store, F](IsWriting(filler))
  }
  object Ops {
    implicit def toOps[F[_]](implicit I: Inject[Store, F]): Ops[F] = new Ops[F]
  }
}

/**
  * Path
  * ----
  * The file wrapper, we can get a file from a Path.
  */
import cats.Eval
import java.io.File
sealed trait Path {
  def file: Eval[File]
}
object Path {
  def apply(p: String): Path = new Path {
    def file: Eval[File] = Eval.later(new File(p))
  }
}

/**
  * Filler
  * ------
  * The ufs3 store's alias. The ufs3's store is just like some blank host list, and save
  * file is just like filling blank. That's the name is from.
  */
sealed trait Filler {
}

/**
  * FileMode
  * --------
  * The mode used to Read/Write a file. 
  * - ReadOnly: the file opened readonly
  * - ReadWrite: the file opened readable and writable
  */
sealed trait FileMode {
  def mode: String
  override def toString(): String = mode
}
object FileMode {
  case object ReadOnly extends FileMode {
    val mode: String = "r"
  }
  case object ReadWrite extends FileMode {
    val mode: String = "rw"
  }
}

/**
  * Size
  * ----
  * Wrapper of Long, to identity the file length in bytes
  */
sealed trait Size{
  def sizeInByte: Long
}

object Size {
  sealed trait ToLong[A] {
    def toLong(a: A): Long
  }
  final class SizeOp[A](a: A)(implicit ev: ToLong[A]) {
    def B: Size = new Size {
      override def sizeInByte: Long = ev.toLong(a)
    }
    def MiB: Size = new Size {
      override def sizeInByte: Long = ev.toLong(a) * 1024
    }
  }

  implicit def AnyToLong[A] = new ToLong[A] {
    override def toLong(a: A): Long = a.toString.toLong
  }

  implicit def toSizeOp[A](a: A)(implicit ev: ToLong[A]) = new SizeOp[A](a)
}

/**
  * Data
  * ----
  * Wrapper of data block
  */
sealed trait Data{
}
