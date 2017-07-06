package store

/**
  * Created by songwenchao on 2017/7/5.
  */
object Response {

  def safeResponse[A](a: ⇒ A): Either[Throwable, A] =
    try {
      Right(a)
    } catch {
      case t: Throwable ⇒ Left(t)
    }

}
