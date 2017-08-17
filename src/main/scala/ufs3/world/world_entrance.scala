package ufs3.world

import java.io.{FileInputStream, FileOutputStream}

import org.slf4j.LoggerFactory
import ufs3.integ._
import ufs3.world.args.parser
import ufs3.world.commons._
import ufs3.world.utils.render._

object Entrance extends App { self ⇒

  //val start = System.currentTimeMillis()
  ufs3.integ.commons.init()
  //println(s"init cost ${System.currentTimeMillis() - start} ms")


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
                getArg     ← x.getArg
                toSaveFile ← getArg.toSaveFile
                key        ← getArg.key
              } yield {
                val out = new FileOutputStream(toSaveFile)
                try {
                  get(getArg.asConfig, key, out).unsafeRun()
                } finally {
                  out.close()
                }
              }
            case Command.List ⇒
              for {
                listArg ← x.listArg
              } yield {
                val idxs = list(listArg.asConfig, 0, "asc").unsafeRun()
                renderIdxs(idxs)(println)
              }
            case f if f == Command.FreeFildexSpace || f == Command.FreeFillerSpace ⇒
              for {
                freeSpaceArg ← x.freeSpaceArg
              } yield {
                val freeSpaceTask = f match {
                  case Command.FreeFillerSpace ⇒ free.filler(freeSpaceArg.asConfig)
                  case Command.FreeFildexSpace ⇒ free.fildex(freeSpaceArg.asConfig)
                  case _                       ⇒ throw new IllegalStateException("not possible")
                }
                val freeSpace = freeSpaceTask.unsafeRun()
                renderSize(freeSpace, freeSpaceArg.unit)(println)
              }

            case Command.Repair ⇒
              for {
                repairArg ← x.repairArg
              } yield {
                repair(repairArg.asConfig).unsafeRun()
              }
            case Command.HttpServer ⇒
              for {
                httpServerArg ← x.httpServerArg
              } yield {
                httpServer(httpServerArg).unsafeRun()
              }
          }
        case None ⇒
      }
    case _ ⇒
  }

}
