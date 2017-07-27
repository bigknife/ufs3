/**
  * command_get.scala
  * ----------
  * get file command implementation
  * @author bigknife
  * @since 2017/7/21
  */
package ufs3
package integration
package command

import java.io._
import cats.data.{Coproduct, Kleisli}
import cats.effect.IO
import sop._
import ufs3.integration.config.UniConfig
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import ufs3.integration.interpreter._
import ufs3.kernel.fildex.Fildex.Idx
import ufs3.kernel.sandwich.SandwichOut

import scala.util.Try
import ufs3.core.data.Data._
import ufs3.core.open.OpenProgram._
import ufs3.core.read.ReadProgram._
import ufs3.core.fetch.FetchProgroam._
import ufs3.core.shutdown.ShutdownProgram._
trait GetCommand {
  type App1[A]       = Coproduct[Block.Op, Filler.Op, A]
  type App2[A]       = Coproduct[Log.Op, App1, A]
  type StartupApp[A] = Coproduct[Fildex.Op, App2, A]

  type ReadApp[A] = Coproduct[SandwichOut.Op[OutputStream, ?], StartupApp, A]

  private val getInterpreter: NT[ReadApp, Kleisli[IO, UniConfig, ?]] = {
    sandwichOutInterpreter or
      (fildexInterperter or
        (logInterperter or
          (blockInterperter or fillerInterperter)))
  }

  private def getWithKeyProg(coreConfig: CoreConfig, key: String, to: OutputStream): SOP[ReadApp, Unit] =
    for {
      ufs3 ← openForRead[ReadApp].run(coreConfig)
      _    ← readWithKey[ReadApp, OutputStream](key, ufs3, to).run(coreConfig)
      _    ← shutdown[ReadApp](ufs3).run(coreConfig)
    } yield ()

  private[command] def _runWithKey(coreConfig: CoreConfig, key: String, out: OutputStream): Try[Unit] = {
    val prog        = getWithKeyProg(coreConfig, key, out)
    val interpreter = getInterpreter
    Try {
      prog.foldMap(interpreter).run(UniConfig()).unsafeRunSync()
    }
  }

  private[command] def _idxOfKey(coreConfig: CoreConfig, key: String): Try[Option[Idx]] = {
    val prog        = idxOfKey[ReadApp](key).run(coreConfig)
    val interpreter = getInterpreter
    Try {
      prog.foldMap(interpreter).run(UniConfig()).unsafeRunSync()
    }
  }

  def getToLocalFileWithKey(coreConfig: CoreConfig, key: String, to: File): Try[Unit] = {
    // create key, md5 of file name
    val outputStream = new BufferedOutputStream(new FileOutputStream(to))
    _runWithKey(coreConfig, key, outputStream).flatMap(_ ⇒ Try { outputStream.close() })
  }

  def existed(coreConfig: CoreConfig, key: String): Try[Boolean] = {
    Try {
      existedKey[ReadApp](key).run(coreConfig).foldMap(getInterpreter).run(UniConfig()).unsafeRunSync()
    }
  }
}
object GetCommand extends GetCommand
