package ufs3.world

import java.io.{File, FileInputStream, FileOutputStream}

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
                  put(putArg.asConfig, key, in).unsafeRun()
                } finally {
                  in.close()
                }
              }
            case Command.Get ⇒
              for {
                getArg ← x.getArg
                toSaveFile ← getArg.toSaveFile
                key ← getArg.key
              } yield {
                val out = new FileOutputStream(toSaveFile)
                try {
                  get(getArg.asConfig, key, out).unsafeRun()
                } finally {
                  out.close()
                }
              }

          }
        case None ⇒
      }
    case _ ⇒
  }
}
