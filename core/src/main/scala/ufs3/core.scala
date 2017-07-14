/**
  * core.scala
  * ----------
  * UFS3 Core program
  * @author bigknife
  * @since 2017/7/14
  */
package ufs3

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
import Sandwich._

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
      } yield ()
      prog
  }

  // write a in: IN to UFS3
  // 1. filler file allocate a start point for new sandwich (key → sandwich), got a start point
  // 1. write sandwich head
  // 2. travese sandwich body and write them
  // 3. write sandwich tail
  def write[F[_], IN](in: IN, out: UFS3)(implicit B: Block[F],
                                         F: Filler[F],
                                         FI: Fildex[F],
                                         S: Sandwich[F, IN]): Kleisli[Id, CoreConfig, SOP[F, Unit]] = Kleisli {
    coreConfig ⇒
      val prog: Id[SOP[F, Unit]] = for {
        startPos ← F.allocate(out.fillerFile)
        _        ← B.seek(out.blockFile, startPos)
        headB    ← S.head()
        _        ← B.write(out.blockFile, headB)
        _ ← {
          def writeBody(): SOP[F, Unit] =
            for {
              obb ← S.nextBody(in)
              _   ← if (obb.nonEmpty) writeBody() else Par.pure[F, Unit](()): SOP[F, Unit] // implicitly transformed by liftPAR_to_SOP
            } yield ()
          writeBody()
        }
        tailB ← S.tail()
        _     ← B.write(out.blockFile, tailB)
      } yield ()
      prog
  }
}
