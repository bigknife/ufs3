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
import ufs3.core.data.Data._
import ufs3.integration.config.UniConfig
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import ufs3.integration.interpreter._
import ufs3.kernel.sandwich.SandwichIn
import ufs3.core.open.OpenProgram._
import ufs3.core.write.WriteProgam._
import scala.util.Try

trait PutCommand {
  type App1[A]       = Coproduct[Block.Op, Filler.Op, A]
  type App2[A]       = Coproduct[Log.Op, App1, A]
  type StartupApp[A] = Coproduct[Fildex.Op, App2, A]

  type WriteApp[A] = Coproduct[SandwichIn.Op[InputStream, ?], StartupApp, A]

  private val putInterpreter: NT[WriteApp, Kleisli[IO, UniConfig, ?]] = {
    sandwichInInterpreter or
      (fildexInterperter or
        (logInterperter or
          (blockInterperter or fillerInterperter)))
  }

  private def putProg(coreConfig: CoreConfig, key: String, ins: InputStream): SOP[WriteApp, String] =
    for {
      ufs3 ← openForWrite[WriteApp].run(coreConfig)
      uuid    ← write[WriteApp, InputStream](key, ins, ufs3).run(coreConfig)
    } yield uuid

  def _run(coreConfig: CoreConfig, key: String, ins: InputStream): Try[String] = {
    val prog = putProg(coreConfig, key, ins)
    val interpreter = putInterpreter
    Try {
      prog.foldMap(interpreter).run(UniConfig()).unsafeRunSync()
    }
  }

  def repeat(x: String, n: Int): String = {
    if (n <= 1) x
    else x + repeat(x, n -1)
  }
  def to32str(str: String): String = {
    if (str.length > 32) str.substring(0, 32)
    else repeat("0", 32 - str.length) + str
  }

  // return (key, uuid) tuple2
  def writeLocalFile(coreConfig: CoreConfig, file: File, key: Option[String]): Try[(String, String)] = {
    // create key, md5 of file name
    val _key =  key.map(to32str).getOrElse(MessageDigest.getInstance("md5").digest(file.getAbsolutePath.getBytes("UTF-8"))
      .map("%02x" format _).mkString(""))
    _run(coreConfig, _key, new FileInputStream(file)).map(x ⇒ (_key, x))
  }
}
object PutCommand extends PutCommand
