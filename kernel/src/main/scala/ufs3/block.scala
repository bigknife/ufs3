/**
  * block.scala
  * ----------
  * the underlying big block file.
  * @author bigknife
  * @since 2017/7/6
  */
package ufs3
package kernel
package block

import java.nio.ByteBuffer

import scala.language.higherKinds
import scala.language.implicitConversions
import cats.free.{Free, Inject}
import cats.Eval
import Free._

/**
  * Block to lift to Free monad
  */
sealed trait Block[A]
object Block {
  type Response[A] = Either[Throwable, A]

  //some util methods for response
  private[kernel] def freeError[F[_], A](t: Throwable): Free[F, Response[A]] =
    Free.pure[F, Response[A]](Left(t))
  private[kernel] def freeResponse[F[_], A](a: A): Free[F, Response[A]] =
    Free.pure[F, Response[A]](Right(a))

  // Block Command
  final case class Open(path: Path, mode: FileMode)       extends Block[Response[Option[BlockFile]]]
  final case class Close(bf: BlockFile)                   extends Block[Response[Unit]]
  final case class Create(path: Path, size: Size)         extends Block[Response[BlockFile]]
  final case class Delete(path: Path)                     extends Block[Response[Unit]]
  final case class Seek(bf: BlockFile, pos: Long)         extends Block[Response[Unit]]
  final case class Read(bf: BlockFile, size: Size)        extends Block[Response[ByteBuffer]]
  final case class Write(bf: BlockFile, data: ByteBuffer) extends Block[Response[Unit]]
  final case class Lock(bf: BlockFile)                    extends Block[Response[Boolean]]
  final case class Unlock(bf: BlockFile)                  extends Block[Response[Unit]]

  final class Ops[F[_]](implicit I: Inject[Block, F]) {
    // abrr of inject
    private[this] def ij[A](b: Block[Response[A]]): Free[F, Response[A]] = inject[Block, F](b)

    def open(path: Path, mode: FileMode): Free[F, Response[Option[BlockFile]]]  = ij[Option[BlockFile]](Open(path, mode))
    def close(bf: BlockFile): Free[F, Response[Unit]]                   = ij[Unit](Close(bf))
    def create(path: Path, size: Size): Free[F, Response[BlockFile]]    = ij[BlockFile](Create(path, size))
    def delete(path: Path): Free[F, Response[Unit]]                     = ij[Unit](Delete(path))
    def seek(bf: BlockFile, pos: Long): Free[F, Response[Unit]]         = ij[Unit](Seek(bf, pos))
    def read(bf: BlockFile, size: Size): Free[F, Response[ByteBuffer]]  = ij[ByteBuffer](Read(bf, size))
    def write(bf: BlockFile, data: ByteBuffer): Free[F, Response[Unit]] = ij[Unit](Write(bf, data))
    def lock(bf: BlockFile): Free[F, Response[Boolean]]                 = ij[Boolean](Lock(bf))
    def unlock(bf: BlockFile): Free[F, Response[Unit]]             = ij[Unit](Unlock(bf))
  }

  object Ops {
    implicit def ops[F[_]](implicit I: Inject[Block, F]) = new Ops[F]
  }

}

/**
  * BlockFile
  * ---------
  * XFS-friendly big size file
  */
sealed trait BlockFile {}
object BlockFile {
  import java.io.RandomAccessFile
  private[this] final class RandomAccessBlockFile(private val underlying: RandomAccessFile) extends BlockFile {}

  def apply(path: Path, mode: FileMode): Eval[BlockFile] = {
    for {
      f ← path.file
    } yield {
      new RandomAccessBlockFile(underlying = new RandomAccessFile(f, mode.mode))
    }
  }
}

/**
  * Path
  * ----
  * The file wrapper, we can get a file from a Path.
  */
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
  * FileMode
  * --------
  * The mode used to Read/Write a file.
  * - ReadOnly: the file opened readonly
  * - ReadWrite: the file opened readable and writable
  */
sealed trait FileMode {
  def mode: String
  override def toString: String = mode
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
