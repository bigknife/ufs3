/**
  * command_repair.scala
  * ----------
  * repair command implementation
  * @author bigknife
  * @since 2017/7/26
  */
package ufs3
package integration
package command

import cats.data.{Coproduct, Kleisli}
import cats.effect.IO
import core._
import sop.NT
import ufs3.integration.config.UniConfig
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import interpreter._
import sop._

import scala.util.Try

trait RepairCommand {
  type App1[A]      = Coproduct[Block.Op, Filler.Op, A]
  type App2[A]      = Coproduct[Log.Op, App1, A]
  type App3[A]      = Coproduct[Fildex.Op, App2, A]
  type RepairApp[A] = App3[A]

  private val repairIntepreter: NT[RepairApp, Kleisli[IO, UniConfig, ?]] =
    fildexInterperter or (logInterperter or (blockInterperter or fillerInterperter))

  def run(config: CoreConfig): Try[Unit] = Try {
    repairFildex[RepairApp].run(config).foldMap(repairIntepreter).run(UniConfig()).unsafeRunSync()
  }

}
object RepairCommand extends RepairCommand
