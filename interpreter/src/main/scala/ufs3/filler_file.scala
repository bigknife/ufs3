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
import ufs3.interpreter.layout._

import scala.language.implicitConversions

final class RandomFillerFile(tailPosition: Long, private val underlying: RandomAccessBlockFile) extends FillerFile {

  def headBytes: ByteBuffer = {
    val layout = FillerFileLayout(underlying.size(), tailPosition)
    layout.head.byteBuffer
  }



  def tailPos: Long = tailPosition

  def tailMoveTo(pos: Long): Unit = {
    // underlying move to 12, and write pos
    underlying.seek(12)
    import layout.Layout._
    underlying.write(pos.`8Bytes`.byteBuffer, 8)
  }
}

object RandomFillerFile {
  val HEAD_SIZE: Long = FillerFileLayout.HEAD_SIZE.toLong

  def apply(tailPosition: Long, underlying: RandomAccessBlockFile): RandomFillerFile =
    new RandomFillerFile(tailPosition, underlying)

  def magicMatched(bytes: Array[Byte]): Boolean = bytes sameElements FillerFileLayout.HEAD_MAGIC

  implicit def from(ff: FillerFile): RandomFillerFile = ff.asInstanceOf[RandomFillerFile]

}
