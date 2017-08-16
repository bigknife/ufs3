package ufs3.world

import java.io.{File, FileInputStream}

import org.slf4j.LoggerFactory
import ufs3.integ._
import ufs3.world.args.parser
import ufs3.world.commons._

object Entrance extends App { self ⇒
  val log = LoggerFactory.getLogger("entrance")

  log.info("init ufs3...")
  ufs3.integ.commons.init()
  log.info("init ufs3 ok.")


  parser.parse(self.args, Args()) match {
    case Some(x) ⇒
      x.cmd match {
        case Some(c) ⇒
          c match {
            case Command.Create ⇒
              create(x.createArg.get.asConfig).unsafeRun()
            case Command.Put ⇒
              for {
                putArg    ← x.putArg
                localFile ← putArg.localFile
                key       ← putArg.key
              } yield {
                val in = new FileInputStream(localFile)
                try {
                  val task = put(putArg.asConfig, key, in)
                  task.unsafeRun()
                } finally {
                  in.close()
                  println("ok")
                }
              }

          }
        case None ⇒
      }
    case _ ⇒
  }
}
