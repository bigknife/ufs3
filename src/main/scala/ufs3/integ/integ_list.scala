package ufs3.integ

import fs2.Task
import ufs3.kernel.commons.{Config, FileMode, Idx}
import ufs3.kernel.modules.App
import ufs3.prog._
import ufs3.kernel.commons.Order
import freestyle._
import ufs3.interp.commons.Stack

object list {
  def apply(config: Config, limit: Int, order: String): Task[Vector[Idx]] = {
    val app = implicitly[App[App.Op]]
    val p: FreeS[App.Op, Vector[Idx]] = for {
      ufs3 ← open[App.Op](FileMode.ReadOnly)
      idx  ← app.store.fildex.query(limit, Order(order), ufs3.fildexFile.get)
      _    ← close[App.Op](ufs3)
    } yield idx

    Stack.parseApp(p).run(config)
  }
}
