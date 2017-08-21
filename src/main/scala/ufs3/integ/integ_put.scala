package ufs3.integ

import java.io.InputStream
import freestyle.FreeS
import fs2.Task
import ufs3.interp.commons.Stack
import ufs3.kernel.commons.{Config, FileMode}
import ufs3.kernel.modules.App
import ufs3.prog._

object put {
  def apply(config: Config, key: String, in: InputStream): Task[Unit] = {
    val p: FreeS[App.Op, Unit] = for {
      ufs3 ← open[App.Op](FileMode.ReadWrite)
      _    ← write[App.Op](key, in, ufs3)
      _    ← close[App.Op](ufs3)
    } yield ()
    Stack.parseApp(p).run(config)
  }
}