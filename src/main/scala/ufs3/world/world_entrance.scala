package ufs3.world

import java.io.{File, FileInputStream}

import ufs3.integ._
import ufs3.world.args.parser
import ufs3.world.commons._

object Entrance extends App { self ⇒
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
                  println("run put program")
                  val task = put(putArg.asConfig, key, in)
                  println(s"create task: $task")
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
