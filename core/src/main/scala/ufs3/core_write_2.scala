/**
  * core_write_2.scala
  * ----------
  * Write program 2
  * @author bigknife
  * @since 2017/8/4
  */
package ufs3
package core
package write

import cats.Id
import cats.data.Kleisli
import sop.SOP
import ufs3.core.data.Data.{CoreConfig, UFS3}
import ufs3.kernel.backup.Backup
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import ufs3.kernel.sandwich.SandwichIn

import scala.language.higherKinds

trait WriteCommand2 {
  def write[F[_], In](key: String, in: In, out: UFS3)(implicit B: Block[F],
                                                      F: Filler[F],
                                                      FI: Fildex[F],
                                                      BA: Backup[F],
                                                      L: Log[F],
                                                      SI: SandwichIn[F, In]): Kleisli[Id, CoreConfig, SOP[F, Unit]] = Kleisli {config ⇒
    ???
  }
}

object WriteCommand2 {

}