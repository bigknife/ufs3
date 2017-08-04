/**
  * test_log.scala
  * ----------
  *
  * @author bigknife
  * @since 2017/8/4
  */
package ufs3
package kernel
package test
package log

import ufs3.kernel.log.Log
import sop._
import RespSOP._
import cats.Id
import ufs3.kernel.log.Log.Info

object Test extends App {
  val log = Log[Log.Op]

  val prog = for {
    _ ← log.info("轻轻的我走了，").asM
    _ ← log.info("正如我轻轻的来；").asM
    _ ← log.warn("我轻轻的招手，").asM
    _ ← log.info("作别西天的云彩。").asM
  } yield ()

  println(prog)

  val interpreter = new NT[Log.Op, Id] {
    def apply[A](fa: Log.Op[A]): Id[A] = fa match {
      case Info(msg, _) ⇒ Right(println(msg))
      case _ ⇒ (Left(new Exception("un implemented")): Resp[Unit]).asInstanceOf[Id[A]]
    }
  }

  prog.foldMap(interpreter)
}