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

package object core {

  // UFS3 trait
  trait UFS3 {
    def blockFile: BlockFile
    def fillerFile: FillerFile
    def fildexFile: FildexFile
  }

  object UFS3 {
    private[core] def apply(_blockFile: BlockFile, _fillerFile: FillerFile, _fildexFile: FildexFile): UFS3 = new UFS3 {
      val fildexFile: FildexFile = _fildexFile
      val fillerFile: FillerFile = _fillerFile
      val blockFile: BlockFile   = _blockFile
    }
  }

  // core config
  trait CoreConfig {
    def blockPath: Block.Path
    def blockSize: Block.Size
  }

  // startup ufs3
  // 1. open or create a block
  // 2. init or check the block to a filler file
  // 3. create or (check repair) fildex file
  // 4. ok.
  def startup[F[_]](implicit B: Block[F], F: Filler[F], FI: Fildex[F]): Kleisli[Id, CoreConfig, SOP[F, UFS3]] =
    Kleisli { coreConfig ⇒
      val path = coreConfig.blockPath
      val mode = Block.FileMode.ReadWrite
      val size = coreConfig.blockSize
      val prog: Id[SOP[F, UFS3]] = for {
        being      ← B.existed(path)
        blockFile  ← if (being) B.open(path, mode) else B.create(path, size)
        fillerFile ← if (being) F.check(blockFile) else F.check(blockFile)
        fildexFile ← if (being) FI.check(fillerFile) else FI.create(fillerFile)
        _          ← B.lock(blockFile) // lock when startup
      } yield UFS3(blockFile, fillerFile, fildexFile)
      prog
    }

  // shutdown ufs3
  // 1. shutdown filler file
  // 2. shutdown fildex file
  // 3. shutdown block
  // 4. ok
  def shutdown[F[_]](
      ufs3: UFS3)(implicit B: Block[F], F: Filler[F], FI: Fildex[F]): Kleisli[Id, CoreConfig, SOP[F, Unit]] = Kleisli {
    coreConfig ⇒
      val prog: Id[SOP[F, Unit]] = for {
        _ ← F.close(ufs3.fillerFile)
        _ ← FI.close(ufs3.fildexFile)
        _ ← B.close(ufs3.blockFile)
        _ ← B.unlock(ufs3.blockFile)
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
                                                      BAK: Backup[F],
                                                      FI: Fildex[F],
                                                      S: SandwichIn[F, IN]): Kleisli[Id, CoreConfig, SOP[F, Unit]] =
    Kleisli { coreConfig ⇒
      val prog: Id[SOP[F, Unit]] = for {
        startPos ← F.allocate(out.fillerFile)
        _        ← B.seek(out.blockFile, startPos)
        headB    ← S.head()
        _        ← B.write(out.blockFile, headB)
        _        ← BAK.send(headB) //backup
        bodyLength ← {
          def writeBody(): SOP[F, Long] =
            for {
              obb        ← S.nextBody(in)
              nextLength ← if (obb.nonEmpty) writeBody() else Par.pure[F, Long](0): SOP[F, Long] // implicitly transformed by liftPAR_to_SOP
              _          ← if (obb.nonEmpty) BAK.send(obb.get) else Par.pure[F, Unit](()) // backup body
            } yield nextLength + obb.map(_.array().length).getOrElse(0)
          writeBody()
        }
        tailB ← S.tail()
        _     ← B.write(out.blockFile, tailB)
        _     ← BAK.send(tailB)
        _     ← FI.append(out.fildexFile, Idx(key, startPos, headB.array().length + bodyLength + tailB.array().length))
      } yield ()
      prog
    }

  // read by key.
  // 1. read start point of key from index
  // 2. read head
  // 3. read body recursively
  // 4. read tail
  def read[F[_], Out](key: String, bufferSize: Long, from: UFS3, to: Out)(
      implicit B: Block[F],
      F: Filler[F],
      FI: Fildex[F],
      S: SandwichOut[F, Out]): Kleisli[Id, CoreConfig, SOP[F, Unit]] = Kleisli { coreConfig ⇒
    import Size._
    val prog: Id[SOP[F, Unit]] = for {
      optIdx ← FI.fetch(key)
      _ ← if (optIdx.nonEmpty) {
        val startPoint     = optIdx.get.startPoint
        val endPoint       = optIdx.get.endPoint
        val headSize: Long = 0
        val tailSize: Long = 0

        def read(pos: Long, length: Long): SOP[F, ByteBuffer] =
          for {
            _  ← B.seek(from.blockFile, pos)
            bb ← B.read(from.blockFile, length.B)
          } yield bb

        def readBody(pos: Long, length: Long, remain: Long): SOP[F, Unit] =
          for {
            _ ← B.seek(from.blockFile, pos)
            _ ← if (remain > 0) {
              val toRead = Math.min(length, remain)
              for {
                bb ← B.read(from.blockFile, toRead.B)
                _  ← S.outputBody(bb, to)
                _  ← readBody(pos + toRead, length, remain - toRead)
              } yield ()
            } else SOP.pure[F, Unit](())
          } yield ()
        // 1. read head
        // 2. read body recursively
        // 3. read tail
        for {
          headB ← read(startPoint, headSize)
          _     ← S.head(headB)
          _     ← readBody(startPoint + headSize, bufferSize, endPoint - startPoint - headSize - tailSize)
          tailB ← read(endPoint - tailSize, tailSize)
          _     ← S.tail(tailB)
        } yield ()

      } else throw new IOException(s"no key=$key found")
    } yield ()
    prog
  }
}
