package ufs3.integ

import fs2.Task
import ufs3.interp.commons.Stack
import ufs3.kernel.commons.Config
import ufs3.kernel.modules.App

object create {
  def apply(config: Config): Task[Unit] = {
    Stack.parseApp(ufs3.prog.create[App.Op]).run(config)
  }
}