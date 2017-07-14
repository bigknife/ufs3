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

import Filler._
import Fildex._

package object core {

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
  def startup[F[_]](implicit B: Block[F],
                    F: Filler[F],
                    FI: Fildex[F]): Kleisli[Id, CoreConfig, SOP[F, (FillerFile, FildexFile)]] = Kleisli { coreConfig ⇒
    val path = coreConfig.blockPath
    val mode = Block.FileMode.ReadWrite
    val size = coreConfig.blockSize
    val prog: Id[SOP[F, (FillerFile, FildexFile)]] = for {
      being ← B.existed(path)
      blockFile ← if (being) B.open(path, mode) else B.create(path, size)
      fillerFile ← if (being) F.check(blockFile) else F.check(blockFile)
      fildexFile ← if (being) FI.check(fillerFile) else FI.create(fillerFile)
    } yield (fillerFile, fildexFile)
    prog
  }
}