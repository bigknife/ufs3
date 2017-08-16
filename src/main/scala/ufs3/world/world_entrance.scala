package ufs3.world

import ufs3.integ._
import ufs3.world.args.parser
import ufs3.world.commons._

object Entrance extends App {self ⇒
  parser.parse(self.args, Args()) match {
    case Some(x) ⇒ x.cmd match {
      case Some(c) ⇒ c match {
        case Command.Create ⇒
          create(x.createArg.get.asConfig).unsafeRun()
      }
      case None ⇒
    }
    case _ ⇒
  }
}