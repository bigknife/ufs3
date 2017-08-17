package ufs3.kernel

object exceptions {
  abstract class UFS3Exception(msg: String, cause: Option[Throwable] = None) extends Exception(msg, cause.orNull)

  case class KeyNotFoundException(key: String) extends UFS3Exception(s"$key not found in ufs3")
}