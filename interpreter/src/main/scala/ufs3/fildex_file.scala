/**
  * fildex_file.scala
  * ----------
  * the file definition of filedex file
  * @author bigknife
  * @since 2017/7/19
  */
package ufs3
package interpreter
package fildex

import ufs3.interpreter.block.{BlockFileBasedFile, RandomAccessBlockFile}
import ufs3.interpreter.layout.{FildexFileLayout, IdxLayout}
import ufs3.kernel.fildex.Fildex.{FildexFile, Idx}

import scala.annotation.tailrec
import scala.language.implicitConversions

final class RandomFildexFile(private val layout: FildexFileLayout,
                             val underlying: RandomAccessBlockFile,
                             private val indexMap: Map[String, Idx] = Map.empty)
    extends FildexFile
    with BlockFileBasedFile {

  def init(): RandomFildexFile = {
    refreshHead()
    this
  }

  def refreshHead(): Unit = {
    seek(0)
    write(layout.head.byteBuffer)
  }

  def fetchIdx(key: String): Option[Idx] = indexMap.get(key)

  // append
  def append(key: String, idx: Idx): RandomFildexFile = {
    val map = indexMap + (key → idx)

    // write idx data
    val oldTailPos = layout.tailPosition
    seek(oldTailPos.longValue)
    write(idx.byteBuffer)

    val lo = layout.version(layout.version.intValue + 1)// version + 1
      .tailPosition(layout.tailPosition.longValue + IdxLayout.SIZE)// tail position + 24

    // write to idx file head
    seek(0)
    write(lo.head.byteBuffer)

    new RandomFildexFile(lo, underlying, map)
  }

  // load index from idx file
  def loadIndex(): RandomFildexFile = {
    @tailrec
    def _index(start: Long, end: Long, acc: Map[String, Idx]): Map[String, Idx] = {
      if (end > start) {
        underlying.seek(start)
        val bytes48 = new Array[Byte](IdxLayout.SIZE.toInt)
        underlying.read(IdxLayout.SIZE).get(bytes48)
        val idx = IdxLayout.resolveBytes(bytes48)
        val accNew = acc + (idx.key → idx)
        _index(start + IdxLayout.SIZE, end, accNew)
      } else acc
    }
    val index = _index(FildexFileLayout.HEAD_SIZE.toLong, layout.tailPosition.longValue, Map.empty)
    new RandomFildexFile(layout, underlying, index)
  }
}

object RandomFildexFile {
  implicit def from(f: FildexFile): RandomFildexFile = f.asInstanceOf[RandomFildexFile]

  def apply(layout: FildexFileLayout, underlying: RandomAccessBlockFile): RandomFildexFile =
    new RandomFildexFile(layout, underlying)
}
