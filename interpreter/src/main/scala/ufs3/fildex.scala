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
import sop.Resp
import ufs3.interpreter.block._
import ufs3.interpreter.filler._
import ufs3.interpreter.layout._
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex.FildexFile
import ufs3.kernel.fildex._
import ufs3.kernel.filler.Filler.FillerFile

import scala.annotation.tailrec

trait FildexInterpreter extends Fildex.Handler[Kleisli[IO, FildexInterpreter.Config, ?]] {

  import RandomAccessBlockFile._
  def init(ff: Block.BlockFile): Kleisli[IO, FildexInterpreter.Config, Resp[Fildex.FildexFile]] = Kleisli { config ⇒
    IO {
      try {
        val layout = FildexFileLayout(ff.size())
        Right(RandomFildexFile(layout, ff).init())
      } catch {
        case t: Throwable ⇒ Left(t)
      }

    }
  }

  def close(ff: Fildex.FildexFile): Kleisli[IO, FildexInterpreter.Config, Resp[Unit]] =
    Kleisli.pure[IO, FildexInterpreter.Config, Resp[Unit]](Right(()))

  def check(ff: Block.BlockFile, filler: FillerFile): Kleisli[IO, FildexInterpreter.Config, Resp[Boolean]] = Kleisli {
    config ⇒
      IO {
        try {
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
          Right(layout.version.intValue == (filler: RandomFillerFile).version)
        } catch {
          case t: Throwable ⇒ Left(t)
        }
      }
  }

  def load(ff: Block.BlockFile): Kleisli[IO, FildexInterpreter.Config, Resp[Fildex.FildexFile]] = Kleisli { config ⇒
    IO {
      try {
        val headBytes = {
          ff.seek(0)
          val bb    = ff.read(FildexFileLayout.HEAD_SIZE)
          val bytes = new Array[Byte](FildexFileLayout.HEAD_SIZE.toInt)
          bb.get(bytes)
          bytes
        }
        val layout = FildexFileLayout.resolveBytes(headBytes)
        Right(RandomFildexFile(layout, ff).loadIndex())
      } catch {
        case t: Throwable ⇒ Left(t)
      }
    }
  }

  def fetchKey(key: String, fildex: FildexFile): Kleisli[IO, FildexInterpreter.Config, Resp[Option[Fildex.Idx]]] =
    Kleisli { config ⇒
      IO {
        try {
          import RandomFildexFile._
          Right(fildex.fetchIdxWithKey(key))
        } catch {
          case t: Throwable ⇒ Left(t)
        }
      }
    }

  def fetchUuid(uuid: String, fildex: FildexFile): Kleisli[IO, FildexInterpreter.Config, Resp[Option[Fildex.Idx]]] =
    Kleisli { config ⇒
      IO {
        try {
          import RandomFildexFile._
          Right(fildex.fetchIdxWithUuid(uuid))
        } catch {
          case t: Throwable ⇒ Left(t)
        }

      }
    }

  def append(bf: FildexFile, idx: Fildex.Idx): Kleisli[IO, FildexInterpreter.Config, Resp[FildexFile]] = Kleisli {
    config ⇒
      IO {
        try {
          import RandomFildexFile._
          Right(bf.append(idx))
        } catch {
          case t: Throwable ⇒ Left(t)
        }

      }
  }

  def repair(bf: Block.BlockFile, filler: FillerFile): Kleisli[IO, FildexInterpreter.Config, Resp[FildexFile]] =
    Kleisli { config ⇒
      IO {

        try {

          import SandwichLayout._

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
            Right(repairIndex(FillerFileLayout.HEAD_SIZE, fillerFile.tailPos, initIndexFile))
          } else {
            val oldFildexFile = RandomFildexFile(oldLayout, blockFile).loadIndex()
            //fildex version lower than filler version repair data between fildex version and filler version
            val halfVersion = fillerFile.version / 2
            if (oldVersion <= halfVersion) {
              //fildex  version lower than half version repair from head to tail faster
              Right(incrementalRepairIndexFromHead(FillerFileLayout.HEAD_SIZE, 0, oldVersion, oldFildexFile))
            } else {
              //fildex version greater than half version repair from tail to head faster
              Right(incrementalRepairIndexFromTail(fillerFile.tailPos, fillerFile.version, oldVersion, oldFildexFile))
            }
          }
        } catch {
          case t: Throwable ⇒ Left(t)
        }
      }
    }

  def query(limit: Int,
            order: Fildex.Order,
            fildex: FildexFile): Kleisli[IO, FildexInterpreter.Config, Resp[Vector[Fildex.Idx]]] = Kleisli { config ⇒
    IO {
      try {
        import RandomFildexFile._
        Right(fildex.query(limit, order))
      } catch {
        case t: Throwable ⇒ Left(t)
      }

    }
  }

  def freeSpace(fi: FildexFile): Kleisli[IO, FildexInterpreter.Config, Resp[Long]] = Kleisli { config ⇒
    IO {
      try {
        import RandomFildexFile._
        Right(fi.freeSpace)
      } catch {
        case t: Throwable ⇒ Left(t)
      }

    }
  }
}

object FildexInterpreter {

  trait Config {}

  def apply(): FildexInterpreter = new FildexInterpreter {}
}
