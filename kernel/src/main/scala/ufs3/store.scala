/**
  * store.scala
  * -----------
  * the kernel adt of store
  *
  * @author: bigknife@outlook.com
  * @create: 2017/07/03
  */
package ufs3
package kernel
package store

import java.io.RandomAccessFile
import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicLong

import cats.free.{Free, Inject}
import ufs3.kernel.store.FileMode.{ReadOnly, ReadWrite}

import scala.language.{higherKinds, implicitConversions}

/**
  * Store Free Monad
  */
sealed trait Store[A]

object Store {
  type Response[A] = Either[Throwable, A]

  case class Existed(path: Path) extends Store[Response[Boolean]]

  case class IsLegal(filler: Filler) extends Store[Response[Boolean]]

  case class Create(path: Path, size: Size) extends Store[Response[Filler]]

  case class Delete(path: Path) extends Store[Response[Unit]]

  case class Open(path: Path, mode: FileMode) extends Store[Response[Filler]]

  case class Close(filler: Filler) extends Store[Response[Unit]]

  case class Read(filler: Filler, size: Size) extends Store[Response[ReadData]]

  case class Write(filler: WritableFiller, data: WriteData) extends Store[Response[Unit]]

  case class Lock(filler: Filler) extends Store[Response[Unit]]

  case class UnLock(filler: Filler) extends Store[Response[Unit]]

  case class FreeSpace(filler: Filler) extends Store[Response[Size]]

  case class IsWriting(filler: Filler) extends Store[Response[Boolean]]

  case class SeekTo(filler: ReadonlyFiller, position: Position) extends Store[Response[Unit]]

  case class Writable(filler: Filler) extends Store[Response[Option[WritableFiller]]]

  case class Readable(filler: Filler) extends Store[Response[Option[ReadonlyFiller]]]

  class Ops[F[_]](implicit I: Inject[Store, F]) {

    import Free._

    def existed(path: Path): Free[F, Response[Boolean]] = inject[Store, F](Existed(path))

    def isLegal(filler: Filler): Free[F, Response[Boolean]] = inject[Store, F](IsLegal(filler))

    def create(path: Path, size: Size): Free[F, Response[Filler]] = inject[Store, F](Create(path, size))

    def delete(path: Path): Free[F, Response[Unit]] = inject[Store, F](Delete(path))

    def open(path: Path, mode: FileMode): Free[F, Response[Filler]] = inject[Store, F](Open(path, mode))

    def close(filler: Filler): Free[F, Response[Unit]] = inject[Store, F](Close(filler))

    def read(filler: Filler, size: Size): Free[F, Response[ReadData]] = inject[Store, F](Read(filler, size))

    def write(filler: WritableFiller, data: WriteData): Free[F, Response[Unit]] = inject[Store, F](Write(filler, data))

    def lock(filler: Filler): Free[F, Response[Unit]] = inject[Store, F](Lock(filler))

    def unlock(filler: Filler): Free[F, Response[Unit]] = inject[Store, F](UnLock(filler))

    def freeSpace(filler: Filler): Free[F, Response[Size]] = inject[Store, F](FreeSpace(filler))

    def isWriting(filler: Filler): Free[F, Response[Boolean]] = inject[Store, F](IsWriting(filler))

    def seekTo(filler: ReadonlyFiller, position: Position): Free[F, Response[Unit]] =
      inject[Store, F](SeekTo(filler, position))

    def writable(filler: Filler): Free[F, Response[Option[WritableFiller]]] = inject[Store, F](Writable(filler))

    def readable(filler: Filler): Free[F, Response[Option[ReadonlyFiller]]] = inject[Store, F](Readable(filler))
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
import java.io.File

import cats.Eval

sealed trait Path {
  def file: Eval[File]
}

object Path {
  def apply(p: String): Path = new Path {
    def file: Eval[File] = Eval.always(new File(p))
  }
}

/**
  * Filler
  * ------
  * The ufs3 store's alias. The ufs3's store is just like some blank host list, and save
  * file is just like filling blank. That's the name is from.
  */
sealed trait Filler {

  def blockSize: Size

  def path: Path

  def underlying: RandomAccessFile

  override def toString: String = path.file.value.getPath
}

object Filler {
  def apply(filePath: Path, size: Size, mode: FileMode): Filler = {
    val raf = new RandomAccessFile(filePath.file.value.getPath, mode.mode)
    mode match {
      case ReadOnly ⇒
        new ReadonlyFiller {
          override def underlying: RandomAccessFile = raf

          override def blockSize: Size = size

          override def path: Path = filePath
        }
      case ReadWrite ⇒
        new WritableFiller {
          override def underlying: RandomAccessFile = raf

          override def blockSize: Size = size

          override def path: Path = filePath
        }
    }
  }
}

sealed trait WritableFiller extends Filler

sealed trait ReadonlyFiller extends Filler

sealed trait IndexFiller extends Filler

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
sealed trait Size {
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
sealed trait Data

sealed trait WriteData extends Data {
  def content: ByteBuffer
}

object WriteData {
  def apply(buffer: ByteBuffer): WriteData = new WriteData() {
    override def content: ByteBuffer = buffer
  }
}

sealed trait ReadData extends Data {

  private[store] val totalNumber: Long

  private[store] val numberRef: AtomicLong

  def hasRemaining: Boolean

  def next: ByteBuffer
}

object ReadData {
  type TotalNumber     = Long
  type NumberRef       = AtomicLong
  type CurrentPosition = Long
  type TotalSize       = Long

  def apply(f: (TotalNumber, NumberRef, CurrentPosition, TotalSize) ⇒ ByteBuffer,
            size: Size,
            bufferSize: Size,
            startPosition: Position): ReadData = {
    new ReadData {
      private val totalSize                             = size.sizeInByte
      private var currentPosition                       = startPosition.value
      override private[store] val totalNumber: Long     = (totalSize + bufferSize.sizeInByte - 1) / bufferSize.sizeInByte
      override private[store] val numberRef: AtomicLong = new AtomicLong(totalNumber)

      override def next: ByteBuffer = {
        currentPosition =
          if (numberRef.get() == totalNumber) currentPosition else currentPosition + bufferSize.sizeInByte
        f(totalNumber, numberRef, currentPosition, totalSize)
      }

      override def hasRemaining: Boolean = numberRef.get() > 0
    }
  }
}

sealed trait Position {
  def value: Long
}

object Position {
  def apply(v: Long): Position = new Position {
    override def value: Long = v
  }
}
