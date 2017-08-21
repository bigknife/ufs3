package ufs3.kernel

object exceptions {
  abstract class UFS3Exception(msg: String, cause: Option[Throwable] = None) extends Exception(msg, cause.orNull)

  // no key in ufs3
  case class KeyNotFoundException(key: String) extends UFS3Exception(s"$key not found in ufs3")

  // ufs3 is writing
  case object UFS3IsWriting extends UFS3Exception("ufs3 is writing, please retry later")

  // key is duplicated
  case class DuplicatedKey(key: String) extends UFS3Exception(s"$key has existed in ufs3")
}