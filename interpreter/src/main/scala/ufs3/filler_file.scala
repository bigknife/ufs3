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
import ufs3.kernel.filler.Filler.FillerFile
import ufs3.interpreter.layout._

import scala.language.implicitConversions

final class RandomFillerFile(private val layout: FillerFileLayout, private val underlying: RandomAccessBlockFile) extends FillerFile {

  def init(): RandomFillerFile = {
    refreshHead()
    this
  }

  def tailPos: Long = layout.tailPosition.longValue

  def tailPos(pos: Long): RandomFillerFile = {
    import Layout._
    val newLayout = layout.tailPosition(pos.`8Bytes`)
    RandomFillerFile(newLayout, underlying)
  }

  def refreshHead(): Unit = {
    underlying.seek(0)
    underlying.write(layout.head.byteBuffer, FillerFileLayout.HEAD_SIZE)
  }
}

object RandomFillerFile {
  val HEAD_SIZE: Long = FillerFileLayout.HEAD_SIZE.toLong

  def apply(layout: FillerFileLayout, underlying: RandomAccessBlockFile): RandomFillerFile =
    new RandomFillerFile(layout, underlying)

  def magicMatched(bytes: Array[Byte]): Boolean = bytes sameElements FillerFileLayout.HEAD_MAGIC

  implicit def from(ff: FillerFile): RandomFillerFile = ff.asInstanceOf[RandomFillerFile]

}
