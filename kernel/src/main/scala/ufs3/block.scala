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
import java.nio.channels.FileLock

import scala.language.higherKinds
import scala.language.implicitConversions
import cats.free.Inject
import cats.Eval
import sop._

/**
  * Block to lift to Free monad
  */
trait Block[F[_]] {
  import Block._

  def existed(path: Path): Par[F, Boolean]
  def open(path: Path, mode: FileMode): Par[F, BlockFile]
  def close(bf: BlockFile): Par[F, Unit]
  def create(path: Path, size: Size): Par[F, BlockFile]
  def delete(path: Path): Par[F, Unit]
  def seek(bf: BlockFile, pos: Long): Par[F, Unit]
  def read(bf: BlockFile, size: Size): Par[F, ByteBuffer]
  def write(bf: BlockFile, data: ByteBuffer): Par[F, Unit]
  def lock(bf: BlockFile): Par[F, Boolean]
  def unlock(bf: BlockFile): Par[F, Unit]

}
object Block {
  sealed trait Op[A]

  // Block Command
  final case class Existed(path: Path)                    extends Op[Boolean]
  final case class Open(path: Path, mode: FileMode)       extends Op[BlockFile]
  final case class Close(bf: BlockFile)                   extends Op[Unit]
  final case class Create(path: Path, size: Size)         extends Op[BlockFile]
  final case class Delete(path: Path)                     extends Op[Unit]
  final case class Seek(bf: BlockFile, pos: Long)         extends Op[Unit]
  final case class Read(bf: BlockFile, size: Size)        extends Op[ByteBuffer]
  final case class Write(bf: BlockFile, data: ByteBuffer) extends Op[Unit]
  final case class Lock(bf: BlockFile)                    extends Op[Boolean]
  final case class Unlock(bf: BlockFile)                  extends Op[Unit]

  final class To[F[_]](implicit I: Inject[Op, F]) extends Block[F] {

    def existed(path: Path): Par[F, Boolean]                 = liftPar_T[Op, F, Boolean](Existed(path))
    def open(path: Path, mode: FileMode): Par[F, BlockFile]  = liftPar_T[Op, F, BlockFile](Open(path, mode))
    def close(bf: BlockFile): Par[F, Unit]                   = liftPar_T[Op, F, Unit](Close(bf))
    def create(path: Path, size: Size): Par[F, BlockFile]    = liftPar_T[Op, F, BlockFile](Create(path, size))
    def delete(path: Path): Par[F, Unit]                     = liftPar_T[Op, F, Unit](Delete(path))
    def seek(bf: BlockFile, pos: Long): Par[F, Unit]         = liftPar_T[Op, F, Unit](Seek(bf, pos))
    def read(bf: BlockFile, size: Size): Par[F, ByteBuffer]  = liftPar_T[Op, F, ByteBuffer](Read(bf, size))
    def write(bf: BlockFile, data: ByteBuffer): Par[F, Unit] = liftPar_T[Op, F, Unit](Write(bf, data))
    def lock(bf: BlockFile): Par[F, Boolean]                 = liftPar_T[Op, F, Boolean](Lock(bf))
    def unlock(bf: BlockFile): Par[F, Unit]                  = liftPar_T[Op, F, Unit](Unlock(bf))
  }

  implicit def to[F[_]](implicit I: Inject[Op, F]) = new To[F]

  def apply[F[_]](implicit B: Block[F]) = B

  trait Handler[M[_]] extends NT[Op, M] {

    protected[this] def existed(path: Path): M[Boolean]
    protected[this] def open(path: Path, mode: FileMode): M[BlockFile]
    protected[this] def close(bf: BlockFile): M[Unit]
    protected[this] def create(path: Path, size: Size): M[BlockFile]
    protected[this] def delete(path: Path): M[Unit]
    protected[this] def seek(blockFile: BlockFile, pos: Long): M[Unit]
    protected[this] def read(blockFile: BlockFile, size: Size): M[ByteBuffer]
    protected[this] def write(blockFile: BlockFile, data: ByteBuffer): M[Unit]
    protected[this] def lock(blockFile: BlockFile): M[Boolean]
    protected[this] def unlock(blockFile: BlockFile): M[Unit]

    override def apply[A](fa: Op[A]): M[A] = fa match {
      case Existed(path)      ⇒ existed(path)
      case Open(path, mode)   ⇒ open(path, mode)
      case Close(bf)          ⇒ close(bf)
      case Create(path, size) ⇒ create(path, size)
      case Delete(path)       ⇒ delete(path)
      case Seek(bf, pos)      ⇒ seek(bf, pos)
      case Read(bf, size)     ⇒ read(bf, size)
      case Write(bf, data)    ⇒ write(bf, data)
      case Lock(bf)           ⇒ lock(bf)
      case Unlock(bf)         ⇒ unlock(bf)
    }
  }

  /**
    * BlockFile
    * ---------
    * XFS-friendly big size file
    */
  private[ufs3] trait BlockFile {
    /*
    def close(): Unit
    def seek(pos: Long): Unit
    def read(size: Long): ByteBuffer
    def write(data: ByteBuffer, size: Long): Unit
    def lock(): FileLock
    */
  }

  /**
    * Path
    * ----
    * The file wrapper, we can get a file from a Path.
    */
  import java.io.File
  sealed trait Path {outter ⇒
    def file: Eval[File]

    def indexPath: Path = new Path {
      def file: Eval[File] =  outter.file.map(f ⇒ new File(f.getAbsolutePath + ".idx"))
    }
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

}
