package ufs3.integ

import java.io.OutputStream
import fs2.Task
import ufs3.interp.commons.Stack
import ufs3.kernel.commons.{Config, FileMode}
import ufs3.kernel.modules.App
import ufs3.prog._

object get {
  def apply(config: Config, key: String, out: OutputStream): Task[Unit] = {
    val p = for {
      ufs3 ← open[App.Op](FileMode.ReadOnly)
      _ ← read[App.Op](key, ufs3, out)
      _ ← close[App.Op](ufs3)
    } yield ()
    Stack.parseApp[Unit](p).run(config)
  }
}