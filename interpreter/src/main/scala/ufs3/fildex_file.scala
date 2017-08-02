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
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.fildex.Fildex.{FildexFile, Idx}

import scala.annotation.tailrec
import scala.language.implicitConversions

final class RandomFildexFile(private val layout: FildexFileLayout,
                             val underlying: RandomAccessBlockFile,
                             private val uuidMap: Map[String, Idx] = Map.empty, // uuid → idx
                             private val keyUuidMap: Map[String, String] = Map.empty) // key → uuid
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

  def freeSpace: Long = underlying.size() - layout.tailPosition.longValue

  def fetchIdxWithKey(key: String): Option[Idx] = keyUuidMap.get(key).flatMap(uuid ⇒ uuidMap.get(uuid))
  def fetchIdxWithUuid(uuid: String): Option[Idx] = uuidMap.get(uuid)


  // append
  def append(idx: Idx): RandomFildexFile = {
    val _uuidMap = uuidMap + (idx.uuid → idx)
    val _keyUuidMap = keyUuidMap + (idx.key → idx.uuid)

    // write idx data
    val oldTailPos = layout.tailPosition
    seek(oldTailPos.longValue)
    write(idx.byteBuffer)

    val lo = layout.version(layout.version.intValue + 1)// version + 1
      .tailPosition(layout.tailPosition.longValue + IdxLayout.SIZE)// tail position + 24

    // write to idx file head
    seek(0)
    write(lo.head.byteBuffer)

    new RandomFildexFile(lo, underlying, _uuidMap, _keyUuidMap)
  }

  // load index from idx file
  def loadIndex(): RandomFildexFile = {
    @tailrec
    def _index(start: Long, end: Long, accUuidMap: Map[String, Idx], accKeyUuidMap: Map[String, String]): (Map[String, Idx], Map[String, String]) = {
      if (end > start) {
        underlying.seek(start)
        val bytes48 = new Array[Byte](IdxLayout.SIZE.toInt)
        underlying.read(IdxLayout.SIZE).get(bytes48)
        val idx = IdxLayout.resolveBytes(bytes48)
        val accUuidMapNew = accUuidMap + (idx.uuid → idx)
        val accKeyUuidNew = accKeyUuidMap + (idx.key → idx.uuid)
        _index(start + IdxLayout.SIZE, end, accUuidMapNew, accKeyUuidNew)
      } else (accUuidMap, accKeyUuidMap)
    }
    val index = _index(FildexFileLayout.HEAD_SIZE.toLong, layout.tailPosition.longValue, Map.empty, Map.empty)
    new RandomFildexFile(layout, underlying, index._1, index._2)
  }

  def query(limit: Int, order: Fildex.Order): Vector[Idx] = {
    // todo return all idx now, should be fixed in next version
    // should use a query-friendly data structer to store the index
    // problem: 仅记住了uuid最后一个的位置
    keyUuidMap.toVector.map {
      case (key, uuid) ⇒
        val idx = uuidMap(uuid)
        Idx(key, uuid, idx.startPoint, idx.endPoint)
    }
  }
}

object RandomFildexFile {
  implicit def from(f: FildexFile): RandomFildexFile = f.asInstanceOf[RandomFildexFile]

  def apply(layout: FildexFileLayout, underlying: RandomAccessBlockFile): RandomFildexFile =
    new RandomFildexFile(layout, underlying)
}
