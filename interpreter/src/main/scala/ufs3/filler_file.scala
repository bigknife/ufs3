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

import sun.jvm.hotspot.runtime.Bytes
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
object Layout {
  type Bytes = Array[Byte]

  abstract class FixedLengthBytes(len: Int) {outter ⇒
    require(bytes.length == len, s"bytes length should be $len")
    def bytes: Bytes
    def ++(n: FixedLengthBytes): FixedLengthBytes = new FixedLengthBytes() {
      def bytes: Bytes =  outter.bytes ++ n.bytes
    }
  }

  case class `4Bytes`(bytes: Bytes) extends FixedLengthBytes(4)
  case class `8Bytes`(bytes: Bytes) extends FixedLengthBytes(8)
  case class `32Bytes`(bytes: Bytes) extends FixedLengthBytes(32)
  case class `100Bytes`(bytes: Bytes) extends FixedLengthBytes(100)

  def longToBytes(l: Long): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putLong(l)
    bb.array()
  }
}

trait FillerFileLayout {
  import Layout._
  def magic: `4Bytes` = `4Bytes`("FILL".getBytes("iso8859_1"))
  def createTime: `8Bytes`
  def appInfo: `32Bytes`
  def bodyLength: `8Bytes`
  def version: `8Bytes`
  def versionHash: `32Bytes`
  def versionPos: `8Bytes`

  def head: `100Bytes` = `100Bytes`(
    (magic ++ createTime ++ appInfo ++ bodyLength ++ version ++ versionHash ++ versionPos).bytes
  )
}
