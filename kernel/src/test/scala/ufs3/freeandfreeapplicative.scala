package ufs3
package kernel
package test

import cats.arrow.FunctionK
import cats.data.Coproduct

import scala.language.{higherKinds, implicitConversions}
import cats.free.{Free, FreeApplicative, Inject}
import cats.{Applicative, Id, ~>}

import scala.util.Random

object FFA extends App {
  sealed trait LogOp[A]
  final case class Debug(msg: String) extends LogOp[Unit]
  final case class Info(msg: String) extends LogOp[Int]

  def debug(msg: String): FreeApplicative[LogOp, Unit] = FreeApplicative.lift(Debug(msg))
  def info(msg: String): FreeApplicative[LogOp, Int] = FreeApplicative.lift(Info(msg))

  sealed trait StepOp[A]
  final case class OneStep(s: Int) extends StepOp[Int]
  def oneStep(s: Int): FreeApplicative[StepOp, Int] = FreeApplicative.lift(OneStep(s))

  def oneStepL(s: Int): FreeApplicative[LogOp, Int] = oneStep(s).compile[LogOp](new (StepOp ~> LogOp){
    override def apply[A](fa: StepOp[A]): LogOp[A] = new LogOp[A] {}
  })

  val interpreter = new (LogOp ~> Id) {
    override def apply[A](fa: LogOp[A]): Id[A] = fa match {
      case Debug(msg) ⇒ println(s"[Debug] $msg")
      case Info(msg) ⇒ println(s"[Info] $msg"); new Random().nextInt(100)
      case _ ⇒ println("unknown log"); 0.asInstanceOf[A]
    }
  }
  val oneStepInterpreter = new (StepOp ~> Id) {
    override def apply[A](fa: StepOp[A]): Id[A] = fa match {
      case OneStep(s) ⇒ println(s"OneStep: $s"); s
    }
  }

  val p = debug("Hello,World")
  p.foldMap(interpreter)

  /*
  val interpreterF = new (FreeApplicative[LogOp, ?] ~> Id) {
    override def apply[A](fa: FreeApplicative[LogOp, A]): Id[A] = {
      fa.foldMap(interpreter)
    }
  }
  */

  println("-------------------------------")
  type FS[F[_], A] = Free[FreeApplicative[F, ?], A]
  type Par[F[_], A] = FreeApplicative[F, A]
  implicit def liftPar[F[_], A](fa: FreeApplicative[F, A]): FS[F, A] = Free.liftF[Par[F, ?], A](fa)
  implicit def liftInterpreter[F[_], G[_]: Applicative](fk: FunctionK[F, G]): FunctionK[Par[F, ?], G] = new (Par[F, ?] ~> G) {
    override def apply[A](fa: Par[F, A]): G[A] = fa.foldMap(fk)
  }

  import cats.implicits._


  def debugF(msg: String): Free[FreeApplicative[LogOp, ?], Unit] = liftPar(debug(msg))
  val pf: FS[LogOp, Int] = for {
    _ ← debug("hello,world")
    _ ← (debug("what is the FreeApplicative") |@| debug("en! I don't know")).map[Unit]((x1: Unit, x2: Unit) ⇒ ())
    _ ← debug("Ha! ")
    i ← info("Random number is ?")
    //s ← oneStepL(i) // 是将StepOp 转换为 LogOp, 是否可以转换为更高层的CoProduct
  } yield i

  //尝试将pf转换为coproduct
  type CoApp[A] = Coproduct[LogOp, StepOp, A]
  // 将FS[LogOp, Int] 转换为 FS[CoApp, Int]
  def pfC(implicit I: Inject[LogOp, CoApp]): FS[CoApp, Int] =  pf.compile[Par[CoApp, ?]](new (Par[LogOp, ?] ~> Par[CoApp, ?]) {
    override def apply[A](fa: Par[LogOp, A]): Par[CoApp, A] = fa.compile(new (LogOp ~> CoApp) {
      override def apply[A](fa: LogOp[A]): CoApp[A] = I.inj(fa)
    })
  })

  val pfC1: FS[CoApp, Int] = pfC

  def oneStepC(implicit I: Inject[StepOp, CoApp]): FS[CoApp, Int] = {
    (oneStep(100): FS[StepOp, Int]).compile[Par[CoApp, ?]](new (Par[StepOp, ?] ~> Par[CoApp, ?]) {
      override def apply[A](fa: Par[StepOp, A]): Par[CoApp, A] = fa.compile(new (StepOp ~> CoApp) {
        override def apply[A](fa: StepOp[A]): CoApp[A] = I.inj(fa)
      })
    })
  }
  val osC1: FS[CoApp, Int] = oneStepC

  val interpreterF = liftInterpreter(interpreter)
  val oneStepInterpreterF = liftInterpreter(oneStepInterpreter)

  val coInterpreterF: Par[CoApp, ?] ~> Id  = liftInterpreter[CoApp, Id](interpreter or oneStepInterpreter)

  println(pf.foldMap(interpreterF))

  println("-------------------THIS IS COAPP-------------------------")
  val coApp: FS[CoApp, Unit] = for {
    _ ← pfC1
    _ ← osC1
  } yield ()

  // 怎样使用隐式转换而不需要标注类型呢？
  coApp.foldMap[Id]((interpreter or oneStepInterpreter): CoApp ~> Id)
}