package ufs3.interp

object commons {
  import _root_.fs2.Task
  import cats.data.Kleisli
  import ufs3.kernel.commons.Config

  type Stack[A] = Kleisli[Task, Config, A]

  object Stack {
    def delay[A](f: Config ⇒ A) = Kleisli { (config: Config) ⇒
      //Task.now(f(config))
      Task.delay(f(config))
    }
    def configless[A](f: ⇒ A): Stack[A] = Kleisli {_ ⇒
      //Task.now(f)
      Task.delay(f)
    }

    import ufs3.kernel.modules.App
    import freestyle._
    def parseApp[A](p: FreeS[App.Op, A]): Stack[A] = _interpApp.interpretApp(p)
  }

  private trait _interpApp {
    import freestyle._
    import freestyle.implicits._
    import freestyle.effects.error.implicits._
    import _root_.fs2.interop.cats._
    import ufs3.interp.block._
    import ufs3.interp.fildex._
    import ufs3.interp.filler._
    import ufs3.interp.sandwich._
    import ufs3.interp.log._
    import ufs3.interp.byteBufferStream._
    import ufs3.kernel.modules._
    import rd.implicits._
    import ufs3.kernel.commons.Config
    import _root_.fs2.Task

    def interpretAppToTask[A](p: FreeS[App.Op, A], config: Config): Task[A] = p.interpret[Stack].run(config)
    def interpretApp[A](p: FreeS[App.Op, A]): Stack[A] = p.interpret[Stack]
  }

  private object _interpApp extends _interpApp

}
