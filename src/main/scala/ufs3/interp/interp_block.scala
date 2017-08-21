package ufs3.interp
import java.io.File
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock}
import java.util.concurrent.ConcurrentHashMap

import cats.Eval
import ufs3.kernel.algebras.Block
import ufs3.kernel.commons._

import scala.language.implicitConversions

object block {
  import java.io.RandomAccessFile

  import RandomAccessBlockFile._
  import commons._

  implicit val blockInterp: Block.Handler[Stack] = new Block.Handler[Stack] {

    private[this] lazy val lockMap = new ConcurrentHashMap[BlockFile, FileLock]

    protected[this] def existed(path: Path): Stack[Boolean] =
      Stack.configless[Boolean](new File(path.file.value.getPath).exists())


    protected[this] def open(path: Path, mode: FileMode): Stack[BlockFile] =
      Stack.configless[BlockFile](RandomAccessBlockFile(path, mode).value)

    protected[this] def close(bf: BlockFile): Stack[Unit] =
      Stack.configless[Unit](bf.close())


    protected[this] def create(path: Path, size: Size): Stack[BlockFile] =
      Stack.configless[BlockFile](RandomAccessBlockFile(path, FileMode.ReadWrite).map(x ⇒ x.size(size.sizeInByte)).value)


    protected[this] def delete(path: Path): Stack[Unit] =
      Stack.configless[Unit] {new File(path.file.value.getPath).delete(); ()}


    protected[this] def seek(bf: BlockFile, pos: Long): Stack[Unit] =
      Stack.configless[Unit](bf.seek(pos))

    protected[this] def read(bf: BlockFile, size: Size): Stack[ByteBuffer] =
      Stack.configless[ByteBuffer](bf.read(size.sizeInByte))

    protected[this] def write(bf: BlockFile, data: ByteBuffer): Stack[Unit] =
      Stack.configless[Unit](bf.write(data))

    protected[this] def lock(bf: BlockFile): Stack[Boolean] = Stack.configless[Boolean] {
        val optLock = Option(lockMap.get(bf))
        val lock = optLock.getOrElse(bf.lock())
        if (lock == null) false
        else {
          lockMap.putIfAbsent(bf, lock)
          true
        }
      }

    protected[this] def unlock(bf: BlockFile): Stack[Unit] = Stack.configless[Unit] {
      Option(lockMap.get(bf)).foreach { lock ⇒
        lock.release()
        lockMap.remove(lock)
      }
    }
  }

  /**
    * BlockFile
    * ---------
    * XFS-friendly big size file
    */
  private[interp] final class RandomAccessBlockFile(private val underlying: RandomAccessFile) extends BlockFile {
    def close(): Unit         = underlying.close()
    def seek(pos: Long): Unit = underlying.seek(pos)
    def read(size: Long): ByteBuffer =
      underlying.getChannel.map(FileChannel.MapMode.READ_ONLY, underlying.getChannel.position(), size)

    // write size of data
    def write(data: ByteBuffer): Unit = {
      if (data.limit() != 0 && size > 0) {
        if (data.position() != 0) data.flip()

        underlying.getChannel
          .map(FileChannel.MapMode.READ_WRITE, underlying.getChannel.position(), data.limit().toLong)
          .put(data)
        ()

      } else ()
    }
    def lock(): FileLock = underlying.getChannel.tryLock()
    def size(): Long = underlying.length()
    //reset file size
    def size(l: Long): RandomAccessBlockFile = {
      underlying.setLength(l)
      this
    }

  }
  object RandomAccessBlockFile {
    def apply(path: Path, mode: FileMode): Eval[BlockFile] = {
      for {
        f ← path.file
      } yield {
        new RandomAccessBlockFile(underlying = new RandomAccessFile(f, mode.mode))
      }
    }
    // assure that in `interpreter` scope, the `BlockFile`'s sub type is only RandomAccessBlockFile
    implicit def from(b: BlockFile): RandomAccessBlockFile = b.asInstanceOf[RandomAccessBlockFile]
  }
  private [interp] trait BlockFileBasedFile {
    def underlying: RandomAccessBlockFile

    def seek(position: Long): Unit = underlying.seek(position)
    //def write(bb: ByteBuffer, size: Int): Unit = underlying.write(bb, size)
    def write(bb: ByteBuffer): Unit = underlying.write(bb)
  }
}