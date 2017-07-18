/**
  * filler_file.scala
  * ----------
  * FillerFile implementation
  * @author bigknife
  * @since 2017/7/18
  */
package ufs3
package interpreter
package filler

import java.nio.ByteBuffer

import ufs3.interpreter.block.RandomAccessBlockFile
import ufs3.kernel.block.Block.BlockFile
import ufs3.kernel.filler.Filler.FillerFile

import scala.language.implicitConversions

final class RandomFillerFile(tailPosition: Long, private val underlying: RandomAccessBlockFile) extends FillerFile {
  import RandomFillerFile._

  def headBytes: ByteBuffer = {
    val bb = ByteBuffer.allocate(20)
    bb.put(HEAD_MAGIC)
    bb.putLong(underlying.size())
    bb.putLong(tailPosition)
    bb
  }

  def tailPos: Long = tailPosition

  def tailMoveTo(pos: Long): Unit = {
    // underlying move to 12, and write pos
    underlying.seek(12)
    val bb = ByteBuffer.allocate(8)
    bb.putLong(pos)
    underlying.write(bb, 8)
  }
}

object RandomFillerFile {
  private val HEAD_MAGIC = "FILL".getBytes("iso8859_1")
  val HEAD_SIZE: Long    = 20 // 4magic, + 8blocksize, + 8tailposition

  def apply(tailPosition: Long, underlying: RandomAccessBlockFile): RandomFillerFile =
    new RandomFillerFile(tailPosition, underlying)

  def magicMatched(bytes: Array[Byte]): Boolean = bytes sameElements HEAD_MAGIC

  implicit def from(ff: FillerFile): RandomFillerFile = ff.asInstanceOf[RandomFillerFile]

}
