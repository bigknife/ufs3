/**
  * core.scala
  * ----------
  * UFS3 Core program
  * @author bigknife
  * @since 2017/7/14
  */
package ufs3

import java.io.IOException
import java.nio.ByteBuffer
import java.security.MessageDigest
import java.util.concurrent.atomic.AtomicReference

import cats.data.Kleisli
import cats.Id

import scala.language.higherKinds
import sop._
import kernel.block._
import kernel.fildex._
import kernel.filler._
import kernel.sandwich._
import Filler._
import Fildex._
import Block._
import ufs3.kernel.backup.Backup
import ufs3.kernel.log.Log

package object core {

  // UFS3 trait
  trait UFS3 {
    def blockFile: AtomicReference[BlockFile]
    def fillerFile: AtomicReference[FillerFile]
    def fildexFile: AtomicReference[FildexFile]
  }

  object UFS3 {
    private[core] def apply(_blockFile: BlockFile, _fillerFile: FillerFile, _fildexFile: FildexFile): UFS3 = new UFS3 {
      val fildexFile: AtomicReference[FildexFile] = new AtomicReference[FildexFile](_fildexFile)
      val fillerFile: AtomicReference[FillerFile] = new AtomicReference[FillerFile](_fillerFile)
      val blockFile: AtomicReference[BlockFile]   = new AtomicReference[BlockFile](_blockFile)
    }
  }

  // core config
  trait CoreConfig {
    def fillerBlockPath: Block.Path
    def fillerBlockSize: Block.Size
    def idxBlockSize: Block.Size
    def fillerReadBufferSize: Block.Size
  }

  // startup ufs3
  // 1. open or create a block
  // 2. init or check the block to a filler file
  // 3. create or (check repair) fildex file
  // 4. ok.
  def startup[F[_]](implicit B: Block[F],
                    F: Filler[F],
                    FI: Fildex[F],
                    L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, UFS3]] =
    Kleisli { coreConfig ⇒
      val path    = coreConfig.fillerBlockPath
      val mode    = Block.FileMode.ReadWrite
      val size    = coreConfig.fillerBlockSize
      val idxSize = coreConfig.idxBlockSize

      val pathString = path.file.value.getAbsolutePath

      import L._

      val prog: Id[SOP[F, UFS3]] = for {
        _     ← debug(s"existing $pathString ?")
        being ← B.existed(path)
        _     ← debug(s"${if (being) "existed" else "not existed"} $pathString")
        bfFiller ← if (being) {
          for {
            _ ← debug(s"open $pathString")
            a ← B.open(path, mode)
          } yield a
        } else {
          for {
            _ ← debug(s"creating $pathString")
            a ← B.create(path, size)
            _ ← debug(s"created $pathString, size=$size")
          } yield a
        }
        bfFildex ← if (being) {
          for {
            _ ← debug(s"open index for $pathString")
            a ← B.open(path.indexPath, mode)
          } yield a

        } else {
          for {
            _ ← debug(s"creating index for $pathString")
            a ← B.create(path.indexPath, idxSize)
            _ ← debug(s"created index for $pathString")
          } yield a
        }
        fillerFile ← if (being) {
          for {
            _ ← debug(s"checking filler file: $pathString")
            a ← F.check(bfFiller)
          } yield a

        } else {
          for {
            _ ← debug(s"initializing filler file: $pathString")
            a ← F.init(bfFiller)
            _ ← debug(s"initialized filler file: $pathString")
          } yield a
        }
        fildexFile ← if (being) {
          for {
            _      ← debug("checking filler index file")
            passed ← FI.check(bfFildex, fillerFile)
            a ← if (passed) {
              for {
                _ ← debug(s"checking passed. load filler index for $pathString")
                b ← FI.load(bfFildex)
              } yield b

            } else {
              for {
                _ ← debug(s"checking rejected! repair filler index for $pathString")
                b ← FI.repair(bfFildex, fillerFile)
              } yield b

            }
          } yield a
        } else {
          for {
            _ ← debug(s"init filler index file for $pathString")
            a ← FI.init(bfFildex)
          } yield a
        }
        _ ← debug(s"lock file: $pathString")
        _ ← B.lock(bfFiller) // lock when startup
      } yield UFS3(bfFiller, fillerFile, fildexFile)
      prog
    }

  // shutdown ufs3
  // 1. shutdown filler file
  // 2. shutdown fildex file
  // 3. shutdown block
  // 4. ok
  def shutdown[F[_]](ufs3: UFS3)(implicit B: Block[F],
                                 F: Filler[F],
                                 FI: Fildex[F],
                                 L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Unit]] =
    Kleisli { coreConfig ⇒
      import L._
      val prog: Id[SOP[F, Unit]] = for {
        _ ← debug("close filler file")
        _ ← F.close(ufs3.fillerFile.get())
        _ ← debug("close fildex file")
        _ ← FI.close(ufs3.fildexFile.get())
        _ ← debug("unlock block file")
        _ ← B.unlock(ufs3.blockFile.get())
        _ ← debug("close block file")
        _ ← B.close(ufs3.blockFile.get())
      } yield ()
      prog
    }

  // write a in: IN to UFS3
  // 1. filler file allocate a start point for new sandwich (key → sandwich), got a start point
  // 1. write sandwich head
  // 2. travese sandwich body and write them
  // 3. write sandwich tail
  // Done: no index, no backup, no lock will be fixed at #17: add backup logic to core dsl
  def write[F[_], IN](key: String, length: Long, in: IN, out: UFS3)(
      implicit B: Block[F],
      F: Filler[F],
      //BAK: Backup[F],
      L: Log[F],
      FI: Fildex[F],
      S: SandwichIn[F, IN]): Kleisli[Id, CoreConfig, SOP[F, Unit]] =
    Kleisli { coreConfig ⇒
      import L._
      def prog(md: MessageDigest): Id[SOP[F, Unit]] =
        for {
          optIdx ← FI.fetch(key, out.fildexFile.get())
          startPos ← if (optIdx.isEmpty) F.startAppend(out.fillerFile.get())
          else throw new IllegalArgumentException(s"$key has existed in ufs3")
          _     ← B.seek(out.blockFile.get(), startPos)
          headB ← S.head(key, length)
          _     ← B.write(out.blockFile.get(), headB)
          //_        ← BAK.send(headB) //backup
          bodyLength ← {
            def writeBody(md5: MessageDigest, pos: Long): SOP[F, Long] =
              for {
                obb ← S.nextBody(in)
                nextLength ← if (obb.nonEmpty) {
                  md5.update(obb.get)
                  for {
                    _ ← B.seek(out.blockFile.get(), pos)
                    _ ← B.write(out.blockFile.get(), obb.get)
                    a ← writeBody(md5, pos + obb.get.limit())
                  } yield a
                } else
                  Par.pure[F, Long](0): SOP[F, Long] // implicitly transformed by liftPAR_to_SOP
                //_ ← if (obb.nonEmpty) BAK.send(obb.get) else Par.pure[F, Unit](()) // backup body
              } yield nextLength + obb.map(_.limit()).getOrElse(0)
            // 从 startpos + headB 的地方写入
            writeBody(md, startPos + headB.limit())
          }
          _     ← debug(s"writed $bodyLength bytes")
          _     ← debug(md.digest().map("%02x".format(_)).mkString(""))
          tailB ← S.tail(md.digest().map("%02x".format(_)).mkString("").getBytes("iso8859_1"), bodyLength)
          _     ← B.seek(out.blockFile.get(), startPos + headB.limit() + bodyLength)
          _     ← B.write(out.blockFile.get(), tailB)
          //_     ← BAK.send(tailB)
          newFildexFile ← FI.append(out.fildexFile.get(),
                                    Idx(key, startPos, startPos + headB.limit() + bodyLength + tailB.limit()))
          newFillerFile ← F.endAppend(out.fillerFile.get(),
                                      startPos,
                                      startPos + headB.limit() + bodyLength + tailB.limit())
          _ ← debug(s"writed. startPos: $startPos, endPos: ${startPos + headB.limit() + bodyLength + tailB
            .limit()}, ${headB.limit()} $bodyLength ${tailB.limit()}")
        } yield {
          out.fillerFile.set(newFillerFile)
          out.fildexFile.set(newFildexFile)
          ()
        }
      prog(MessageDigest.getInstance("md5"))
    }

  // read by key.
  // 1. read start point of key from index
  // 2. read head
  // 3. read body recursively
  // 4. read tail
  def read[F[_], Out](key: String, from: UFS3, to: Out)(implicit B: Block[F],
                                                        F: Filler[F],
                                                        FI: Fildex[F],
                                                        S: SandwichOut[F, Out],
                                                        L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Unit]] =
    Kleisli { coreConfig ⇒
      import Size._
      import L._
      val prog: Id[SOP[F, Unit]] = for {
        optIdx ← FI.fetch(key, from.fildexFile.get())
        _      ← debug(s"key($key) idx is $optIdx")
        _ ← if (optIdx.nonEmpty) {
          val startPoint = optIdx.get.startPoint
          val endPoint   = optIdx.get.endPoint

          def read(pos: Long, length: Long): SOP[F, ByteBuffer] =
            for {
              _  ← B.seek(from.blockFile.get(), pos)
              bb ← B.read(from.blockFile.get(), length.B)
            } yield bb

          def readBody(pos: Long, length: Long, remain: Long): SOP[F, Unit] =
            for {
              _ ← B.seek(from.blockFile.get(), pos)
              _ ← if (remain > 0) {
                val toRead = Math.min(length, remain)
                for {
                  bb ← B.read(from.blockFile.get(), toRead.B)
                  _  ← S.outputBody(bb, to)
                  _  ← readBody(pos + toRead, length, remain - toRead)
                } yield ()
              } else SOP.pure[F, Unit](())
            } yield ()
          // 1. read head
          // 2. read body recursively
          // 3. read tail
          for {
            headSize ← S.headSize()
            tailSize ← S.tailSize()
            _        ← debug(s"start: ${startPoint + headSize}, remain: ${endPoint - startPoint - headSize - tailSize}")
            headB    ← read(startPoint, headSize)
            _        ← S.head(headB, to)
            _ ← readBody(startPoint + headSize,
                         coreConfig.fillerReadBufferSize.sizeInByte,
                         endPoint - startPoint - headSize - tailSize)
            tailB ← read(endPoint - tailSize, tailSize)
            _     ← S.tail(tailB, to)
          } yield ()

        } else throw new IOException(s"no key=$key found")
      } yield ()
      prog
    }
}
