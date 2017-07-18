package ufs3
package interpreter
package layout

import java.nio.ByteBuffer
import scala.language.implicitConversions

object Layout {
  type Bytes = Array[Byte]

  abstract class FixedLengthBytes(val len: Int) { outter ⇒
    require(bytes.length == len, s"bytes length should be $len")
    def bytes: Bytes
    def byteBuffer: ByteBuffer = ByteBuffer.wrap(bytes)

    def ++(n: FixedLengthBytes): FixedLengthBytes = new FixedLengthBytes(len + n.len) {
      def bytes: Bytes = outter.bytes ++ n.bytes
    }

    override def toString: String = s"${len}Bytes"
  }

  case class `4Bytes`(bytes: Bytes)   extends FixedLengthBytes(4)
  case class `8Bytes`(bytes: Bytes)   extends FixedLengthBytes(8)
  case class `32Bytes`(bytes: Bytes)  extends FixedLengthBytes(32)
  case class `60Bytes`(bytes: Bytes)  extends FixedLengthBytes(60)
  case class `68Bytes`(bytes: Bytes)  extends FixedLengthBytes(68)
  case class `100Bytes`(bytes: Bytes) extends FixedLengthBytes(100)

  def longToBytes(l: Long): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putLong(l)
    bb.array()
  }

  class Op[A](a: A)(implicit ev: A ⇒ Bytes) {
    def `4Bytes`: Layout.`4Bytes`     = Layout.`4Bytes`(ev(a))
    def `8Bytes`: Layout.`8Bytes`     = Layout.`8Bytes`(ev(a))
    def `32Bytes`: Layout.`32Bytes`   = Layout.`32Bytes`(ev(a))
    def `60Bytes`: Layout.`60Bytes`   = Layout.`60Bytes`(ev(a))
    def `68Bytes`: Layout.`68Bytes`   = Layout.`68Bytes`(ev(a))
    def `100Bytes`: Layout.`100Bytes` = Layout.`100Bytes`(ev(a))
  }
  implicit def toOp[A](a: A)(implicit ev: A ⇒ Bytes) = new Op[A](a)
  // instances
  implicit val long2BytesInst: Long ⇒ Bytes  = x ⇒ longToBytes(x)
  implicit val str2BytesInst: String ⇒ Bytes = x ⇒ x.getBytes("utf-8")
}

trait FillerFileLayout {
  import Layout._
  def magic: `4Bytes` = `4Bytes`(FillerFileLayout.HEAD_MAGIC)
  def blockSize: `8Bytes`
  def tailPosition: `8Bytes`
  def version: `8Bytes`
  def versionHash: `32Bytes`
  def versionPos: `8Bytes`

  def head: `68Bytes` = `68Bytes`(
    (magic ++ blockSize ++ tailPosition ++ version ++ versionHash ++ versionPos).bytes
  )
}

object FillerFileLayout {
  val HEAD_MAGIC: Array[Byte] = "FILL".getBytes("iso8859_1")
  val HEAD_SIZE = 68
  def apply(
      _blockSize: Long,
      _tailPosition: Long,
      _version: Long = 0,
      _versionHash: String = "",
      _versionPos: Long = 60
  ): FillerFileLayout =
    new FillerFileLayout {
      import Layout._

      def blockSize: Layout.`8Bytes`   = _blockSize.`8Bytes`
      def tailPosition: Layout.`8Bytes`   = _tailPosition.`8Bytes`
      def version: Layout.`8Bytes`      = _version.`8Bytes`
      def versionHash: Layout.`32Bytes` = _versionHash.`32Bytes`
      def versionPos: Layout.`8Bytes`   = _versionPos.`8Bytes`
    }
}
