package ufs3.interp

object commons {
  import _root_.fs2.Task
  import cats.data.Kleisli
  import ufs3.kernel.commons.Config

  type Stack[A] = Kleisli[Task, Config, A]

  object Stack {
    def now[A](f: Config ⇒ A) = Kleisli {(config: Config) ⇒
      Task.now(f(config))
    }
    def configless[A](f: ⇒ A): Stack[A] = Kleisli {_ ⇒
      Task.now(f)
    }
  }


}