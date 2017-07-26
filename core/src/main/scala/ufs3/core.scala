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

  private def open[F[_]](mode: Block.FileMode)(implicit B: Block[F],
                                               F: Filler[F],
                                               FI: Fildex[F],
                                               L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, UFS3]] = {
    Kleisli { config ⇒
      val path       = config.fillerBlockPath
      val pathString = path.file.value.getAbsolutePath
      val prog: Id[SOP[F, UFS3]] = for {
        being ← B.existed(path)
        _     ← if (!being) throw new IOException(s"file not exists: $pathString") else SOP.pure[F, Unit](())
        bf    ← B.open(path, mode)
        bfi   ← B.open(path.indexPath, mode)
        ff    ← F.check(bf)
        idxOk ← FI.check(bfi, ff)
        fif   ← if (idxOk) FI.load(bfi) else throw new IllegalStateException("fildex is not legal, please repair it")
      } yield UFS3(bf, ff, fif)
      prog
    }
  }

  def openForWrite[F[_]](implicit B: Block[F],
                         F: Filler[F],
                         FI: Fildex[F],
                         L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, UFS3]] = open(FileMode.ReadWrite)

  def openForRead[F[_]](implicit B: Block[F],
                        F: Filler[F],
                        FI: Fildex[F],
                        L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, UFS3]] = open(FileMode.ReadOnly)

  def freeSpaceOfFiller[F[_]](implicit B: Block[F],
                              F: Filler[F],
                              FI: Fildex[F],
                              L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Long]] = {
    openForRead[F].andThen[SOP[F, Long]]((x: SOP[F, UFS3]) ⇒ {
      for {
        ufs3 ← x
        l    ← F.freeSpace(ufs3.fillerFile.get())
      } yield l
    })
  }

  def freeSpaceOfFildex[F[_]](implicit B: Block[F],
                              F: Filler[F],
                              FI: Fildex[F],
                              L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Long]] = {
    openForRead[F].andThen[SOP[F, Long]]((x: SOP[F, UFS3]) ⇒ {
      for {
        ufs3 ← x
        l    ← FI.freeSpace(ufs3.fildexFile.get())
      } yield l
    })
  }

  def existed[F[_]](key: String)(implicit B: Block[F],
                                 F: Filler[F],
                                 FI: Fildex[F],
                                 L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Boolean]] = {
    openForRead[F].andThen[SOP[F, Boolean]]((x: SOP[F, UFS3]) ⇒ {
      for {
        ufs3 ← x
        e    ← FI.fetch(key, ufs3.fildexFile.get())
      } yield e.nonEmpty
    })
  }

  def list[F[_]](limit: Int, order: String)(implicit B: Block[F],
                                            F: Filler[F],
                                            FI: Fildex[F],
                                            L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Vector[Idx]]] = {
    openForRead[F].andThen[SOP[F, Vector[Idx]]]((x: SOP[F, core.UFS3]) ⇒
      for {
        ufs3 ← x
        idx  ← FI.query(limit, Fildex.Order(order), ufs3.fildexFile.get())
      } yield idx)
  }

  def repairFildex[F[_]](implicit B: Block[F],
                         F: Filler[F],
                         FI: Fildex[F],
                         L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Unit]] = {
    Kleisli { config ⇒
      val path       = config.fillerBlockPath
      val pathString = path.file.value.getAbsolutePath

      import L._
      val prog: Id[SOP[F, Unit]] = for {
        being       ← B.existed(path)
        _           ← if (!being) throw new IOException(s"file not exists: $pathString") else SOP.pure[F, Unit](())
        fildexBeing ← B.existed(path.indexPath)
        _ ← (if (fildexBeing) info(s"index file existed: ${path.indexPath.file.value}")
             else
               for {
                 _ ← warn("index file lost, create a new index file now")
                 a ← B.create(path.indexPath, config.idxBlockSize)
                 _ ← FI.init(a)
                 _ ← info(s"a new index file created and initialized: ${path.indexPath.file.value}")
               } yield ()): SOP[F, Unit]

        bf    ← B.open(path, FileMode.ReadWrite)
        bfi   ← B.open(path.indexPath, FileMode.ReadWrite)
        ff    ← F.check(bf)
        idxOk ← FI.check(bfi, ff)
        _ ← (if (idxOk) info("the index file is consistent with filler file, unnecessary to repair.")
             else {
               for {
                 _ ← info("start to repair the index file")
                 _ ← FI.repair(bfi, ff)
                 _ ← info("repair successfully!")
               } yield ()
             }): SOP[F, Unit]
      } yield ()
      prog
    }
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
  def shutdown[F[_]](
      ufs3: UFS3)(implicit B: Block[F], F: Filler[F], FI: Fildex[F], L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Unit]] =
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
  def write[F[_], IN](key: String, in: IN, out: UFS3)(implicit B: Block[F],
                                                      F: Filler[F],
                                                      //BAK: Backup[F],
                                                      L: Log[F],
                                                      FI: Fildex[F],
                                                      S: SandwichIn[F, IN]): Kleisli[Id, CoreConfig, SOP[F, Unit]] =
    Kleisli { coreConfig ⇒
      import L._
      def prog(md: MessageDigest): Id[SOP[F, Unit]] =
        for {
          writing ← F.isWriting(out.fillerFile.get())
          optIdx ← if (writing) {
            for {
              _ ← warn(s"ufs3 is writing, please retry later! requesting key = $key")
            } yield throw new IllegalAccessException("ufs3 is writing, please retry later")
            //throw new IllegalAccessException("ufs3 is writing, please retry later")
          } else FI.fetch(key, out.fildexFile.get())
          _ ← info(s"writing for key: $key")
          startPos ← if (optIdx.isEmpty) F.startAppend(out.fillerFile.get())
          else {
            for {
              _ ← error(s"stop writing, $key has existed in ufs3")
            } yield throw new IllegalArgumentException(s"$key has existed in ufs3")

          }

          headB ← S.head(key, 0) //length can't be detected
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
                    _ ← debug(s"writing $key, from $pos, length: ${obb.get.limit()}")
                  } yield a
                } else
                  Par.pure[F, Long](0): SOP[F, Long] // implicitly transformed by liftPAR_to_SOP
                //_ ← if (obb.nonEmpty) BAK.send(obb.get) else Par.pure[F, Unit](()) // backup body
              } yield nextLength + obb.map(_.limit()).getOrElse(0)
            // 从 startpos + headB 的地方写入
            writeBody(md, startPos +  headB.limit())
          }
          headB ← S.head(key, bodyLength) //length can't be detected
          _     ← B.seek(out.blockFile.get(), startPos)
          _     ← B.write(out.blockFile.get(), headB)

          md5   ← SOP.pure[F, String](md.digest().map("%02x".format(_)).mkString(""))
          _     ← debug(s"writed $bodyLength bytes with key = $key, md5 = $md5")
          tailB ← S.tail(md5.getBytes("iso8859_1"), bodyLength)
          _     ← B.seek(out.blockFile.get(), startPos + headB.limit() + bodyLength)
          _     ← B.write(out.blockFile.get(), tailB)
          //_     ← BAK.send(tailB)
          newFildexFile ← FI.append(out.fildexFile.get(),
                                    Idx(key, startPos, startPos + headB.limit() + bodyLength + tailB.limit()))
          newFillerFile ← F.endAppend(out.fillerFile.get(),
                                      startPos,
                                      startPos + headB.limit() + bodyLength + tailB.limit())
          _ ← info(
            s"writed filler and fildex with key: $key. startPos: $startPos, endPos: ${startPos + headB.limit() + bodyLength + tailB
              .limit()}")
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
