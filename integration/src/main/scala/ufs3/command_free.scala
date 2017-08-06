/**
  * command_free.scala
  * ----------
  * free block and free idx command implementation
  * @author bigknife
  * @since 2017/7/24
  */
package ufs3
package integration
package command

import cats.data.{Coproduct, Kleisli}
import cats.effect.IO
import sop._
import ufs3.integration.config.UniConfig
import ufs3.integration.interpreter._
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import scala.util.Try
import ufs3.core.data.Data._
import ufs3.core.freespace.FreeSpaceProgram._
import cats.implicits._

trait FreeCommand {
  type App1[A]    = Coproduct[Block.Op, Filler.Op, A]
  type App2[A]    = Coproduct[Log.Op, App1, A]
  type App3[A]    = Coproduct[Fildex.Op, App2, A]
  type ListApp[A] = App3[A]

  private val freeSpaceIntepreter: NT[ListApp, Kleisli[IO, UniConfig, ?]] =
    fildexInterperter or (logInterperter or (blockInterperter or fillerInterperter))

  private def freeSpaceOfFillerProg(config: CoreConfig): RespSOP[ListApp, Long] = freeSpaceOfFiller[ListApp].run(config)
  private def freeSpaceOfFildexProg(config: CoreConfig): RespSOP[ListApp, Long] = freeSpaceOfFildex[ListApp].run(config)

  def runFreeSpaceOfFillerProg(config: CoreConfig, u: String): Resp[Unit] =  {
    val freeSpace: Resp[Long] = freeSpaceOfFillerProg(config).foldMap(freeSpaceIntepreter).run(UniConfig()).unsafeRunSync()
    freeSpace.map(x ⇒ println(render(x, u)))
  }

  def runFreeSpaceOfFildexProg(config: CoreConfig, u: String): Resp[Unit] = {
    val freeSpace = freeSpaceOfFildexProg(config).foldMap(freeSpaceIntepreter).run(UniConfig()).unsafeRunSync()
    freeSpace.map(x ⇒ println(render(x, u)))
  }

  private def render(fs: Long, u: String): String = {
    import ufs3.kernel.block.Block.Size._
    fs.B.toStringWithUnit(u)
  }
}
object FreeCommand extends FreeCommand