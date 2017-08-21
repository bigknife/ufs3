package ufs3.interp

import java.nio.ByteBuffer

import ufs3.interp.block.{BlockFileBasedFile, RandomAccessBlockFile}
import ufs3.interp.block.RandomAccessBlockFile._
import ufs3.interp.filler.RandomFillerFile
import ufs3.kernel.commons._

import scala.annotation.tailrec
import scala.language.implicitConversions

object fildex {
  import commons._
  import ufs3.kernel.algebras.Fildex


  implicit val fildexInterp: Fildex.Handler[Stack] = new Fildex.Handler[Stack] {
    protected[this] def init(bf: BlockFile): Stack[FildexFile] = Stack.configless[FildexFile] {
      val layout = FildexFileLayout(bf.size())
      RandomFildexFile(layout, bf).init()
    }

    protected[this] def check(bf: BlockFile, ff: FillerFile): Stack[Boolean] =
      Stack.configless[Boolean] {
        import RandomAccessBlockFile._
        import ufs3.interp.filler.RandomFillerFile._
        val rabf: RandomAccessBlockFile = bf
        val rff: RandomFillerFile = ff

        require(rabf.size() >= FildexFileLayout.HEAD_SIZE,
                s"fildex file length should greater than ${FildexFileLayout.HEAD_SIZE}")
        val headBytes = {
          rabf.seek(0)
          val bb    = rabf.read(FildexFileLayout.HEAD_SIZE)
          val bytes = new Array[Byte](FildexFileLayout.HEAD_SIZE.toInt)
          bb.get(bytes)
          bytes
        }
        val layout = FildexFileLayout.resolveBytes(headBytes)
        // check the version is the same

        layout.version.intValue == rff.version
      }

    protected[this] def repair(bf: BlockFile, ff: FillerFile): Stack[FildexFile] = Stack.configless[FildexFile] {
      import SandwichLayout._

      lazy val blockFile  = RandomAccessBlockFile.from(bf)
      lazy val fillerFile = RandomFillerFile.from(ff)
      lazy val underlying = fillerFile.underlying

      //增量从头部搜索修复索引
      @tailrec
      def incrementalRepairIndexFromHead(startPos: Long,
                                         currentVersion: Int,
                                         oldVersion: Int,
                                         fildexFile: FildexFile): FildexFile = {
        //寻找oldVersion在filler中的位置
        if (currentVersion < oldVersion) {
          underlying.seek(startPos)
          val headBuffer         = underlying.read(SandwichHeadLayout.HEAD_LENGTH.toLong)
          val sandwichHeadLayout = SandwichHeadLayout.resolveBytes(headBuffer)
          incrementalRepairIndexFromHead(
            startPos + SandwichHeadLayout.HEAD_LENGTH + sandwichHeadLayout.bodyLength.longValue + SandwichTailLayout.TAIL_LENGTH,
            currentVersion + 1,
            oldVersion,
            fildexFile)
        } else repairIndex(startPos, fillerFile.tailPos, fildexFile)
      }

      //增量从尾部搜索修复索引
      @tailrec
      def incrementalRepairIndexFromTail(startPos: Long,
                                         currentVersion: Int,
                                         oldVersion: Int,
                                         fildexFile: FildexFile): FildexFile = {
        //寻找oldVersion在filler中的位置
        if (currentVersion > oldVersion) {
          //获取Sandwich的body的length
          underlying.seek(startPos - SandwichTailLayout.TAIL_LENGTH)
          val tailBuffer         = underlying.read(SandwichTailLayout.TAIL_LENGTH.toLong)
          val sandwichTailLayout = SandwichTailLayout.resolveBytes(tailBuffer)
          incrementalRepairIndexFromTail(
            startPos - SandwichTailLayout.TAIL_LENGTH - sandwichTailLayout.bodyLength.longValue - SandwichHeadLayout.HEAD_LENGTH,
            currentVersion - 1,
            oldVersion,
            fildexFile)
        } else repairIndex(startPos, fillerFile.tailPos, fildexFile)
      }

      //修复索引
      @tailrec
      def repairIndex(startPos: Long, endPos: Long, fildex: FildexFile): FildexFile = {
        if (endPos > startPos) {
          underlying.seek(startPos)
          val headBuffer = underlying.read(SandwichHeadLayout.HEAD_LENGTH.toLong)
          val headLayout = SandwichHeadLayout.resolveBytes(headBuffer)
          //计算Sandwich的body的结束位置
          val end    = startPos + SandwichHeadLayout.HEAD_LENGTH + headLayout.bodyLength.longValue + SandwichTailLayout.TAIL_LENGTH
          val buffer = ByteBuffer.allocate(IdxLayout.SIZE.toInt)
          buffer.put(headLayout.key.bytes)
          buffer.putLong(startPos)
          buffer.putLong(end)
          //生成每个Sandwich对应的索引
          val idx = IdxLayout.resolveBytes(buffer.array())
          //写入索引: to store in indexMap and index file also update version and tailPosition int index head
          val indexFile = RandomFildexFile.from(fildex).append(idx)
          repairIndex(end, endPos, indexFile)
        } else fildex
      }

      //fildex old layout head bytes
      val headBytes = {
        blockFile.seek(0)
        val headBuffer = blockFile.read(FildexFileLayout.HEAD_SIZE)
        val headArray  = new Array[Byte](FildexFileLayout.HEAD_SIZE.toInt)
        headBuffer.get(headArray)
        headArray
      }
      val oldLayout  = FildexFileLayout.resolveBytes(headBytes)
      val oldVersion = oldLayout.version.intValue
      if (oldVersion > fillerFile.version) {
        //fildex version greater than filler version full repair index
        val newLayout     = FildexFileLayout(blockFile.size())
        val initIndexFile = RandomFildexFile(newLayout, blockFile).init()
        repairIndex(FillerFileLayout.HEAD_SIZE, fillerFile.tailPos, initIndexFile)
      } else {
        val oldFildexFile = RandomFildexFile(oldLayout, blockFile).loadIndex()
        //fildex version lower than filler version repair data between fildex version and filler version
        val halfVersion = fillerFile.version / 2
        if (oldVersion <= halfVersion) {
          //fildex  version lower than half version repair from head to tail faster
          incrementalRepairIndexFromHead(FillerFileLayout.HEAD_SIZE, 0, oldVersion, oldFildexFile)
        } else {
          //fildex version greater than half version repair from tail to head faster
          incrementalRepairIndexFromTail(fillerFile.tailPos, fillerFile.version, oldVersion, oldFildexFile)
        }
      }
    }

    protected[this] def load(bf: BlockFile): Stack[FildexFile] = Stack.configless[FildexFile]{
      val headBytes = {
        bf.seek(0)
        val bb    = bf.read(FildexFileLayout.HEAD_SIZE)
        val bytes = new Array[Byte](FildexFileLayout.HEAD_SIZE.toInt)
        bb.get(bytes)
        bytes
      }
      val layout = FildexFileLayout.resolveBytes(headBytes)
      RandomFildexFile(layout, bf).loadIndex()
    }

    protected[this] def append(fdf: FildexFile, idx: Idx): Stack[FildexFile] = Stack.configless[FildexFile] {
      import RandomFildexFile._
      fdf.append(idx)
    }

    protected[this] def close(fdf: FildexFile): Stack[Unit] = Stack.configless(())

    protected[this] def fetchKey(key: String, fdf: FildexFile): Stack[Option[Idx]] = Stack.configless[Option[Idx]]{
      import RandomFildexFile._
      fdf.fetchIdxWithKey(key)
    }

    protected[this] def fetchUuid(uuid: String, fdf: FildexFile): Stack[Option[Idx]] = Stack.configless[Option[Idx]] {
      import RandomFildexFile._
      fdf.fetchIdxWithUuid(uuid)
    }

    protected[this] def query(limit: Int, order: Order, fdf: FildexFile): Stack[Vector[Idx]] = Stack.configless[Vector[Idx]]{
      import RandomFildexFile._
      fdf.query(limit, order)
    }

    protected[this] def freeSpace(fdf: FildexFile): Stack[Long] = Stack.configless[Long] {
      import RandomFildexFile._
      fdf.freeSpace
    }
  }


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

    def query(limit: Int, order: Order): Vector[Idx] = {
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
}
