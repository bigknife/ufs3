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
import java.security.MessageDigest

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
import ufs3.kernel.sandwich.SandwichOut

import scala.util.Try

object GetCommand {
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

  private def getProg(coreConfig: CoreConfig, key: String, to: OutputStream): SOP[ReadApp, Unit] =
    for {
      ufs3 ← openForRead[ReadApp].run(coreConfig)
      _    ← read[ReadApp, OutputStream](key, ufs3, to).run(coreConfig)
    } yield ()

  private def _run(coreConfig: CoreConfig, key: String, out: OutputStream): Try[Unit] = {
    val prog        = getProg(coreConfig, key, out)
    val interpreter = getInterpreter
    Try {
      prog.foldMap(interpreter).run(UniConfig()).unsafeRunSync()
    }
  }

  def getToLocalFile(coreConfig: CoreConfig, key: String, to: File): Try[Unit] = {
    // create key, md5 of file name
    val outputStream = new BufferedOutputStream(new FileOutputStream(to))
    _run(coreConfig, key, outputStream).flatMap(_ ⇒ Try{outputStream.close()})
  }
}
