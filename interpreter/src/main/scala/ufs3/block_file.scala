/**
  * block_file.scala
  * ----------
  * BlockFile implementation
  * @author bigknife
  * @since 2017/7/18
  */
package ufs3
package interpreter
package block

import java.io.RandomAccessFile

import ufs3.kernel.block.Block._
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock}

import cats.Eval

import scala.language.implicitConversions

/**
  * BlockFile
  * ---------
  * XFS-friendly big size file
  */
private[interpreter] final class RandomAccessBlockFile(private val underlying: RandomAccessFile) extends BlockFile {
  def close(): Unit         = underlying.close()
  def seek(pos: Long): Unit = underlying.seek(pos)
  def read(size: Long): ByteBuffer =
    underlying.getChannel.map(FileChannel.MapMode.READ_ONLY, underlying.getChannel.position(), size)
  def write(data: ByteBuffer, size: Long): Unit = {
    underlying.getChannel
      .map(FileChannel.MapMode.READ_WRITE, underlying.getChannel.position(), size)
      .put(data); ()
  }
  def lock(): FileLock = underlying.getChannel.tryLock()
  def size(): Long = underlying.length()

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
