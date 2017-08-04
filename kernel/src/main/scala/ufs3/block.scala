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

  def existed(path: Path): RespPar[F, Boolean]
  def open(path: Path, mode: FileMode): RespPar[F, BlockFile]
  def close(bf: BlockFile): RespPar[F, Unit]
  def create(path: Path, size: Size): RespPar[F, BlockFile]
  def delete(path: Path): RespPar[F, Unit]
  def seek(bf: BlockFile, pos: Long): RespPar[F, Unit]
  def read(bf: BlockFile, size: Size): RespPar[F, ByteBuffer]
  def write(bf: BlockFile, data: ByteBuffer): RespPar[F, Unit]
  def lock(bf: BlockFile): RespPar[F, Boolean]
  def unlock(bf: BlockFile): RespPar[F, Unit]

}
object Block {
  sealed trait Op[A]

  // Block Command
  final case class Existed(path: Path)                    extends Op[Resp[Boolean]]
  final case class Open(path: Path, mode: FileMode)       extends Op[Resp[BlockFile]]
  final case class Close(bf: BlockFile)                   extends Op[Resp[Unit]]
  final case class Create(path: Path, size: Size)         extends Op[Resp[BlockFile]]
  final case class Delete(path: Path)                     extends Op[Resp[Unit]]
  final case class Seek(bf: BlockFile, pos: Long)         extends Op[Resp[Unit]]
  final case class Read(bf: BlockFile, size: Size)        extends Op[Resp[ByteBuffer]]
  final case class Write(bf: BlockFile, data: ByteBuffer) extends Op[Resp[Unit]]
  final case class Lock(bf: BlockFile)                    extends Op[Resp[Boolean]]
  final case class Unlock(bf: BlockFile)                  extends Op[Resp[Unit]]

  final class To[F[_]](implicit I: Inject[Op, F]) extends Block[F] {

    def existed(path: Path): RespPar[F, Boolean]                 = liftPar_T[Op, F, Resp[Boolean]](Existed(path))
    def open(path: Path, mode: FileMode): RespPar[F, BlockFile]  = liftPar_T[Op, F, Resp[BlockFile]](Open(path, mode))
    def close(bf: BlockFile): RespPar[F, Unit]                   = liftPar_T[Op, F, Resp[Unit]](Close(bf))
    def create(path: Path, size: Size): RespPar[F, BlockFile]    = liftPar_T[Op, F, Resp[BlockFile]](Create(path, size))
    def delete(path: Path): RespPar[F, Unit]                     = liftPar_T[Op, F, Resp[Unit]](Delete(path))
    def seek(bf: BlockFile, pos: Long): RespPar[F, Unit]         = liftPar_T[Op, F, Resp[Unit]](Seek(bf, pos))
    def read(bf: BlockFile, size: Size): RespPar[F, ByteBuffer]  = liftPar_T[Op, F, Resp[ByteBuffer]](Read(bf, size))
    def write(bf: BlockFile, data: ByteBuffer): RespPar[F, Unit] = liftPar_T[Op, F, Resp[Unit]](Write(bf, data))
    def lock(bf: BlockFile): RespPar[F, Boolean]                 = liftPar_T[Op, F, Resp[Boolean]](Lock(bf))
    def unlock(bf: BlockFile): RespPar[F, Unit]                  = liftPar_T[Op, F, Resp[Unit]](Unlock(bf))
  }

  implicit def to[F[_]](implicit I: Inject[Op, F]) = new To[F]

  def apply[F[_]](implicit B: Block[F]) = B

  trait Handler[M[_]] extends NT[Op, M] {

    protected[this] def existed(path: Path): M[Resp[Boolean]]
    protected[this] def open(path: Path, mode: FileMode): M[Resp[BlockFile]]
    protected[this] def close(bf: BlockFile): M[Resp[Unit]]
    protected[this] def create(path: Path, size: Size): M[Resp[BlockFile]]
    protected[this] def delete(path: Path): M[Resp[Unit]]
    protected[this] def seek(blockFile: BlockFile, pos: Long): M[Resp[Unit]]
    protected[this] def read(blockFile: BlockFile, size: Size): M[Resp[ByteBuffer]]
    protected[this] def write(blockFile: BlockFile, data: ByteBuffer): M[Resp[Unit]]
    protected[this] def lock(blockFile: BlockFile): M[Resp[Boolean]]
    protected[this] def unlock(blockFile: BlockFile): M[Resp[Unit]]

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
  sealed trait Path { outter ⇒
    def file: Eval[File]

    def indexPath: Path = new Path {
      def file: Eval[File] = outter.file.map(f ⇒ new File(f.getAbsolutePath + ".idx"))
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
    def toStringWithUnit(u: String): String = u match {
      case "G" ⇒ "%.2fGiB" format sizeInByte.toDouble / 1024 / 1024 / 1024
      case "M" ⇒ "%.2fMiB" format sizeInByte.toDouble / 1024 / 1024
      case "K" ⇒ "%.2fKiB" format sizeInByte.toDouble / 1024
      case _   ⇒ sizeInByte.toString + "B"
    }
  }

  object Size {
    sealed trait ToLong[A] {
      def toLong(a: A): Long
    }
    final class SizeOp[A](a: A)(implicit ev: ToLong[A]) {
      def B: Size = new Size {
        override def sizeInByte: Long = ev.toLong(a)
      }
      def KiB: Size = new Size {
        def sizeInByte: Long = ev.toLong(a) * 1024
      }
      def MiB: Size = new Size {
        override def sizeInByte: Long = ev.toLong(a) * 1024 * 1024
      }
      def GiB: Size = new Size {
        def sizeInByte: Long = ev.toLong(a) * 1024 * 1024 * 1024
      }
    }

    implicit def AnyToLong[A] = new ToLong[A] {
      override def toLong(a: A): Long = a.toString.toLong
    }

    implicit def toSizeOp[A](a: A)(implicit ev: ToLong[A]) = new SizeOp[A](a)
  }

}
