/**
  * command_put.scala
  * ----------
  * put input stream command implementation
  * @author bigknife
  * @since 2017/7/21
  */
package ufs3
package integration
package command

import java.io.{File, FileInputStream, InputStream}
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
import ufs3.kernel.sandwich.SandwichIn

import scala.util.Try

object PutCommand {
  type App1[A]       = Coproduct[Block.Op, Filler.Op, A]
  type App2[A]       = Coproduct[Log.Op, App1, A]
  type StartupApp[A] = Coproduct[Fildex.Op, App2, A]

  type WriteApp[A] = Coproduct[SandwichIn.Op[InputStream, ?], StartupApp, A]

  private def putInterpreter(): NT[WriteApp, Kleisli[IO, UniConfig, ?]] = {
    sandwichInInterpreter or
      (fildexInterperter or
        (logInterperter or
          (blockInterperter or fillerInterperter)))
  }

  private def putProg(coreConfig: CoreConfig, key: String, ins: InputStream): SOP[WriteApp, Unit] =
    for {
      ufs3 ← openForWrite[WriteApp].run(coreConfig)
      _    ← write[WriteApp, InputStream](key, ins.available().toLong, ins, ufs3).run(coreConfig)
    } yield ()

  private def _run(coreConfig: CoreConfig, key: String, ins: InputStream): Try[Unit] = {
    val prog = putProg(coreConfig, key, ins)
    val interpreter = putInterpreter()
    Try {
      prog.foldMap(interpreter).run(UniConfig()).unsafeRunSync()
    }
  }

  def writeLocalFile(coreConfig: CoreConfig, file: File): Try[String] = {
    // create key, md5 of file name
    val key = MessageDigest.getInstance("md5").digest(file.getAbsolutePath.getBytes("UTF-8"))
      .map("%02x" format _).mkString("")
    _run(coreConfig, key, new FileInputStream(file)).map(_ ⇒ key)
  }
}
