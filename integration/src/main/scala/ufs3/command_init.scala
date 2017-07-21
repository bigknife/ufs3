/**
  * command_init.scala
  * ----------
  * init command implementation
  * @author bigknife
  * @since 2017/7/21
  */
package ufs3
package integration
package command

import cats.arrow.FunctionK
import cats.data.{Coproduct, Kleisli}
import cats.effect.IO
import sop._
import ufs3.core._
import ufs3.integration.config.UniConfig
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import ufs3.integration.interpreter._

import scala.util.Try

object InitCommand {
  type App1[A]       = Coproduct[Block.Op, Filler.Op, A]
  type App2[A]       = Coproduct[Log.Op, App1, A]
  type StartupApp[A] = Coproduct[Fildex.Op, App2, A]

  private def initProg(coreConfig: CoreConfig): SOP[StartupApp, Unit] = for {
    x ← startup[StartupApp].run(coreConfig)
    _ ← shutdown[StartupApp](x).run(coreConfig)
  } yield ()

  private def initInterpreter(): NT[StartupApp, Kleisli[IO, UniConfig, ?]]= {
    fildexInterperter or (logInterperter or (blockInterperter or fillerInterperter))
  }

  def run(coreConfig: CoreConfig): Try[Unit] = {
    val prog = initProg(coreConfig)
    val interpreter = initInterpreter()
    val k = prog.foldMap(interpreter)
    Try{
      k.run(UniConfig()).unsafeRunSync()
    }
  }
}