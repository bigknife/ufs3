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

  // write size of data
  def write(data: ByteBuffer, size: Int): Unit = {
    if (data.limit() != 0 && size > 0) {

      val nbb = ByteBuffer.wrap(data.array())
      nbb.limit(Math.min(data.limit(), size))

      underlying.getChannel
        .map(FileChannel.MapMode.READ_WRITE, underlying.getChannel.position(), nbb.limit().toLong)
        .put(nbb)
      ()

    } else ()
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

private [interpreter] trait BlockFileBasedFile {
  def underlying: RandomAccessBlockFile

  def seek(position: Long): Unit = underlying.seek(position)
  def write(bb: ByteBuffer, size: Int): Unit = underlying.write(bb, size)
  def write(bb: ByteBuffer): Unit = underlying.write(bb, bb.limit())
}