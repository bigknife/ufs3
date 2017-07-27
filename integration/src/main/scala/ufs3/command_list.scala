/**
  * command_list.scala
  * ----------
  * list sandwich item command implementation
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
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.fildex.Fildex.Idx
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import interpreter._
import ufs3.core.data.Data._
import ufs3.core.fetch.FetchProgroam._

import scala.util.Try

trait ListCommand {
  type App1[A]    = Coproduct[Block.Op, Filler.Op, A]
  type App2[A]    = Coproduct[Log.Op, App1, A]
  type App3[A]    = Coproduct[Fildex.Op, App2, A]
  type ListApp[A] = App3[A]

  private val listIntepreter: NT[ListApp, Kleisli[IO, UniConfig, ?]] =
    fildexInterperter or (logInterperter or (blockInterperter or fillerInterperter))
  private def listProg(config: CoreConfig, limit: Int, order: String): SOP[ListApp, Vector[Idx]] =
    list[ListApp](limit, order).run(config)

  def run(config: CoreConfig, limit: Int, order: String): Try[Unit] = {
    Try {
      val idxs = listProg(config, limit, order).foldMap(listIntepreter).run(UniConfig()).unsafeRunSync()
      // render idxs
      print(render(idxs))
    }
  }

  private def render(idxs: Vector[Idx]): String = {
    import ufs3.kernel.block.Block.Size._
    val stringTuples =
      idxs.map(x ⇒ (x.key, x.uuid, x.fileLength.B.toStringWithUnit("M"), x.startPoint.toString, x.endPoint.toString))
    //find max length
    val maxLengthTuples =
      stringTuples.foldLeft(("key".length, "uuid".length, "size".length, "start".length, "end".length)) { (acc, n) ⇒
        (Math.max(acc._1, n._1.length),
         Math.max(acc._2, n._2.length),
         Math.max(acc._3, n._3.length),
         Math.max(acc._4, n._4.length),
         Math.max(acc._5, n._5.length))
      }

    def strFilling(str: String, length: Int): String = {
      if (str.length < length) strFilling(s"$str ", length - 1)
      else str
    }

    def row(x: (String, String, String, String, String)) =
      s"${strFilling(x._1, maxLengthTuples._1)}\t" +
        s"${strFilling(x._2, maxLengthTuples._2)}\t" +
        s"${strFilling(x._3, maxLengthTuples._3)}\t" +
        s"${strFilling(x._4, maxLengthTuples._4)}\t" +
        s"${strFilling(x._5, maxLengthTuples._5)}\r\n"
    val head = row(("key", "uuid", "size", "start", "end"))
    val body = stringTuples.map(row)
    head + body.mkString("")
  }

}

object ListCommand extends ListCommand
