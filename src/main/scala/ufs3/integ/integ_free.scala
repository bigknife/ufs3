package ufs3.integ

import fs2.Task
import ufs3.interp.commons.Stack
import ufs3.kernel.commons.{Config, FileMode}
import ufs3.kernel.modules.App
import ufs3.prog._
import freestyle._

object free {
  private val app = implicitly[App[App.Op]]

  object fildex {
    def apply(config: Config): Task[Long] = {
      val p: FreeS[App.Op, Long] = for {
        ufs3 ← open[App.Op](FileMode.ReadOnly)
        l ← app.store.fildex.freeSpace(ufs3.fildexFile.get)
        _ ← close[App.Op](ufs3)
      } yield l

      Stack.parseApp(p).run(config)
    }
  }

  object filler {
    def apply(config: Config): Task[Long] = {
      val p: FreeS[App.Op, Long] = for {
        ufs3 ← open[App.Op](FileMode.ReadOnly)
        l ← app.store.filler.freeSpace(ufs3.fillerFile.get)
        _ ← close[App.Op](ufs3)
      } yield l

      Stack.parseApp(p).run(config)
    }
  }
}