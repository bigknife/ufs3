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
import java.util.concurrent.atomic.AtomicReference

import cats.data.{Coproduct, Kleisli}
import cats.effect.IO
import sop._
import ufs3.core.data.Data
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
import cats.implicits._
import RespSOP._
import scala.util.{Failure, Success, Try}

trait PutCommand {
  type App1[A]       = Coproduct[Block.Op, Filler.Op, A]
  type App2[A]       = Coproduct[Log.Op, App1, A]
  type StartupApp[A] = Coproduct[Fildex.Op, App2, A]

  type WriteApp[A] = Coproduct[SandwichIn.Op[InputStream, ?], StartupApp, A]

  // cache the writable ufs3
  private val ufs3Cache = new AtomicReference[Option[UFS3]](None)

  private val putInterpreter: NT[WriteApp, Kleisli[IO, UniConfig, ?]] = {
    sandwichInInterpreter or
      (fildexInterperter or
        (logInterperter or
          (blockInterperter or fillerInterperter)))
  }

  def writableUfs3(coreConfig: CoreConfig): Resp[UFS3] = {
    if (ufs3Cache.get().isDefined) Right(ufs3Cache.get().get)
    else {
      val p: RespSOP[WriteApp, UFS3] = openForWrite[WriteApp].run(coreConfig)
      val _ufs3: Resp[Data.UFS3]     = p.foldMap(putInterpreter).run(UniConfig()).unsafeRunSync()
      _ufs3.map(x ⇒ ufs3Cache.set(Some(x)))
      _ufs3
    }
  }

  private def putProg(coreConfig: CoreConfig, key: String, ins: InputStream): RespSOP[WriteApp, String] =
    for {
      ufs3 ← openForWrite[WriteApp].run(coreConfig).asM
      uuid ← write[WriteApp, InputStream](key, ins, ufs3).run(coreConfig).asM
      _    ← forceToWrite[WriteApp](ufs3).run(coreConfig).asM
    } yield uuid

  def _run(coreConfig: CoreConfig, key: String, ins: InputStream): Resp[String] = {
    val prog        = putProg(coreConfig, key, ins)
    val interpreter = putInterpreter
    prog.foldMap(interpreter).run(UniConfig()).unsafeRunSync()
  }

  def _ufs3IsWriting(coreConfig: CoreConfig, ufs3: UFS3): Resp[Unit] = {
    val p = isWritable[WriteApp](ufs3).run(coreConfig)
    p.foldMap(putInterpreter).run(UniConfig()).unsafeRunSync()
  }

  def _ufs3ExistedKey(coreConfig: CoreConfig, key: String, ufs3: UFS3): Resp[Boolean] = {
    val p = existedKey[WriteApp](key, ufs3).run(coreConfig)
    p.foldMap(putInterpreter).run(UniConfig()).unsafeRunSync()
  }

  def _runWithUfs3(coreConfig: CoreConfig, key: String, ins: InputStream, ufs3: UFS3): Resp[String] = {
    val p: RespSOP[WriteApp, String] = write[WriteApp, InputStream](key, ins, ufs3).run(coreConfig)
    val s                            = p.foldMap(putInterpreter).run(UniConfig()).unsafeRunSync()
    s.left.foreach {
      t: Throwable ⇒
        val p: RespSOP[WriteApp, Unit] = forceToWrite[WriteApp](ufs3).run(coreConfig)
        p.foldMap(putInterpreter).run(UniConfig()).unsafeRunSync()
    }
    s
  }

  def repeat(x: String, n: Int): String = {
    if (n <= 1) x
    else x + repeat(x, n - 1)
  }
  def to32str(str: String): String = {
    if (str.length > 32) str.substring(0, 32)
    else repeat("0", 32 - str.length) + str
  }

  // return (key, uuid) tuple2
  def writeLocalFile(coreConfig: CoreConfig, file: File, key: Option[String]): Resp[(String, String)] = {
    // create key, md5 of file name
    val _key = key
      .map(to32str)
      .getOrElse(
        MessageDigest
          .getInstance("md5")
          .digest(file.getAbsolutePath.getBytes("UTF-8"))
          .map("%02x" format _)
          .mkString(""))
    _run(coreConfig, _key, new FileInputStream(file)).map(x ⇒ (_key, x))
  }
}
object PutCommand extends PutCommand
