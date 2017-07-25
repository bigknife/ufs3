/**
  * fildex.scala
  * ----------
  * interpreter of Fildex
  *
  * @author bigknife
  * @since 2017/7/18
  */
package ufs3
package interpreter
package fildex

import java.nio.ByteBuffer

import cats.data.Kleisli
import cats.effect.IO
import ufs3.interpreter.block._
import ufs3.interpreter.filler._
import ufs3.interpreter.layout.{FildexFileLayout, FillerFileLayout, IdxLayout}
import ufs3.interpreter.sandwich.Sandwich
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex.FildexFile
import ufs3.kernel.fildex._
import ufs3.kernel.filler.Filler.FillerFile

import scala.annotation.tailrec

trait FildexInterpreter extends Fildex.Handler[Kleisli[IO, FildexInterpreter.Config, ?]] {

  import RandomAccessBlockFile._
  def init(ff: Block.BlockFile): Kleisli[IO, FildexInterpreter.Config, Fildex.FildexFile] = Kleisli { config ⇒
    IO {
      val layout = FildexFileLayout(ff.size())
      RandomFildexFile(layout, ff).init()
    }
  }

  def close(ff: Fildex.FildexFile): Kleisli[IO, FildexInterpreter.Config, Unit] =
    Kleisli.pure[IO, FildexInterpreter.Config, Unit](())

  def check(ff: Block.BlockFile, filler: FillerFile): Kleisli[IO, FildexInterpreter.Config, Boolean] = Kleisli {
    config ⇒
      IO {
        // check the blockFile size if gt index file head
        require(ff.size() >= FildexFileLayout.HEAD_SIZE,
                s"fildex file length should greater than ${FildexFileLayout.HEAD_SIZE}")

        val headBytes = {
          ff.seek(0)
          val bb    = ff.read(FildexFileLayout.HEAD_SIZE)
          val bytes = new Array[Byte](FildexFileLayout.HEAD_SIZE.toInt)
          bb.get(bytes)
          bytes
        }

        val layout = FildexFileLayout.resolveBytes(headBytes)

        // check the version is the same
        import RandomFillerFile._
        layout.version.intValue == (filler: RandomFillerFile).version
      }
  }

  def load(ff: Block.BlockFile): Kleisli[IO, FildexInterpreter.Config, Fildex.FildexFile] = Kleisli { config ⇒
    IO {
      val headBytes = {
        ff.seek(0)
        val bb    = ff.read(FildexFileLayout.HEAD_SIZE)
        val bytes = new Array[Byte](FildexFileLayout.HEAD_SIZE.toInt)
        bb.get(bytes)
        bytes
      }
      val layout = FildexFileLayout.resolveBytes(headBytes)
      RandomFildexFile(layout, ff).loadIndex()
    }
  }

  def fetch(key: String, fildex: FildexFile): Kleisli[IO, FildexInterpreter.Config, Option[Fildex.Idx]] = Kleisli {
    config ⇒
      IO {
        import RandomFildexFile._
        fildex.fetchIdx(key)
      }
  }

  def append(bf: FildexFile, idx: Fildex.Idx): Kleisli[IO, FildexInterpreter.Config, FildexFile] = Kleisli { config ⇒
    IO {
      import RandomFildexFile._
      bf.append(idx.key, idx)
    }
  }

  def repair(bf: Block.BlockFile, filler: FillerFile): Kleisli[IO, FildexInterpreter.Config, FildexFile] = Kleisli {
    config ⇒
      IO {

        lazy val blockFile  = RandomAccessBlockFile.from(bf)
        lazy val fillerFile = RandomFillerFile.from(filler)
        lazy val underlying = fillerFile.underlying

        //增量从头部搜索修复索引
        @tailrec
        def incrementalRepairIndexFromHead(startPos: Long,
                                           currentVersion: Int,
                                           oldVersion: Int,
                                           fildexFile: FildexFile): FildexFile = {
          //寻找oldVersion在filler中的位置
          if (currentVersion < oldVersion) {
            //忽略Sandwich中head magic + create time
            underlying.seek(startPos + Sandwich.HEAD_LENGTH - 8)
            val bodyLengthBuffer = underlying.read(8)
            val bodyLength       = bodyLengthBuffer.getLong
            incrementalRepairIndexFromHead(startPos + Sandwich.HEAD_LENGTH + bodyLength + Sandwich.HASH_SIZE + 8,
                                           currentVersion + 1,
                                           oldVersion,
                                           fildexFile)
          } else repairIndex(startPos, fillerFile.tailPos, fildexFile)
        }

        //增量从尾部搜索修复索引
        @tailrec
        def incrementalRepairIndexFromTail(startPos: Long, // 5 2
                                           currentVersion: Int,
                                           oldVersion: Int,
                                           fildexFile: FildexFile): FildexFile = {
          //寻找oldVersion在filler中的位置
          if (currentVersion > oldVersion) {
            //获取Sandwich的body的length
            underlying.seek(startPos - 8)
            val bodyLengthBuffer = underlying.read(8)
            val bodyLength       = bodyLengthBuffer.getLong
            incrementalRepairIndexFromTail(startPos - Sandwich.HASH_SIZE - 8 - bodyLength - Sandwich.HEAD_LENGTH,
                                           currentVersion - 1,
                                           oldVersion,
                                           fildexFile)
          } else repairIndex(startPos, fillerFile.tailPos, fildexFile)
        }

        //修复索引
        @tailrec
        def repairIndex(startPos: Long, endPos: Long, fildex: FildexFile): FildexFile = {
          if (endPos > startPos) {
            //忽略Sandwich前12个字节:head magic + create time
            underlying.seek(startPos + FildexInterpreter.SANDWICH_MAGIC_TIME_SIZE)
            //从Sandwich中读取name of hex string
            val keyArray  = new Array[Byte](FildexInterpreter.SANDWICH_KEY_SIZE.toInt)
            val keyBuffer = underlying.read(FildexInterpreter.SANDWICH_KEY_SIZE)
            keyBuffer.get(keyArray)
            val key = new String(keyArray, "utf-8")
            //从Sandwich中读取body length
            underlying.seek(startPos + Sandwich.HEAD_LENGTH - 8)
            val bodyLengthBuffer = underlying.read(8)
            val bodyLength       = bodyLengthBuffer.getLong
            //计算Sandwich的body的结束位置
            val end    = startPos + Sandwich.HEAD_LENGTH + bodyLength + Sandwich.HASH_SIZE + 8
            val buffer = ByteBuffer.allocate(IdxLayout.SIZE.toInt)
            buffer.put(keyArray)
            buffer.putLong(startPos)
            buffer.putLong(end)
            //生成每个Sandwich对应的索引
            val idx = IdxLayout.resolveBytes(buffer.array())
            //写入索引: to store in indexMap and index file also update version and tailPosition int index head
            val indexFile = RandomFildexFile.from(fildex).append(key, idx)
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
  }
}

object FildexInterpreter {

  lazy val SANDWICH_MAGIC_TIME_SIZE: Long = 12 // head magic and create time occupy 12 bytes
  lazy val SANDWICH_KEY_SIZE: Long        = 32 // key of sandwich name hex string occupy 32 bytes
  lazy val SANDWICH_BODY_SIZE: Long       = 8  // sandwich body occupy 8 bytes

  trait Config {}

  def apply(): FildexInterpreter = new FildexInterpreter {}
}
