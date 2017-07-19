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

import ufs3.interpreter.block.{BlockFileBasedFile, RandomAccessBlockFile}
import ufs3.kernel.filler.Filler.FillerFile
import ufs3.interpreter.layout._

import scala.language.implicitConversions

final class RandomFillerFile(private val layout: FillerFileLayout, val underlying: RandomAccessBlockFile)
    extends FillerFile
    with BlockFileBasedFile {

  def init(): RandomFillerFile = {
    refreshHead()
    this
  }

  // current tail position
  def tailPos: Long = layout.tailPosition.longValue

  // set current tail position
  def tailPos(pos: Long): RandomFillerFile = {
    import Layout._
    val newLayout = layout.tailPosition(pos.`8Bytes`)
    RandomFillerFile(newLayout, underlying)
  }

  // refresh head in the file
  def refreshHead(): RandomFillerFile = {
    seek(0)
    write(layout.head.byteBuffer)
    this
  }

  // get current version
  def version: Int = layout.version.intValue


}

object RandomFillerFile {

  def apply(layout: FillerFileLayout, underlying: RandomAccessBlockFile): RandomFillerFile =
    new RandomFillerFile(layout, underlying)

  def magicMatched(bytes: Array[Byte]): Boolean = bytes sameElements FillerFileLayout.HEAD_MAGIC

  implicit def from(ff: FillerFile): RandomFillerFile = ff.asInstanceOf[RandomFillerFile]

}
