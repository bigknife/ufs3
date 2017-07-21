package ufs3
package interpreter
package layout

import java.nio.ByteBuffer

import ufs3.kernel.fildex.Fildex.Idx

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

    def longValue: Long = byteBuffer.getLong
    def intValue: Int = byteBuffer.getInt

    override def toString: String = s"${len}Bytes"
  }
  // int
  case class `4Bytes`(bytes: Bytes) extends FixedLengthBytes(4)
  type Int_Bytes = `4Bytes`

  // long
  case class `8Bytes`(bytes: Bytes) extends FixedLengthBytes(8)
  type Long_Bytes = `8Bytes`

  case class `24Bytes`(bytes: Bytes) extends FixedLengthBytes(24)

  // md5...
  case class `32Bytes`(bytes: Bytes) extends FixedLengthBytes(32)

  // others
  case class `60Bytes`(bytes: Bytes)  extends FixedLengthBytes(60)
  case class `64Bytes`(bytes: Bytes)  extends FixedLengthBytes(64)
  case class `100Bytes`(bytes: Bytes) extends FixedLengthBytes(100)

  def longToBytes(l: Long): Array[Byte] = {
    val bb = ByteBuffer.allocate(8)
    bb.putLong(l)
    bb.array()
  }
  def intToBytes(i: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(i)
    bb.array()
  }

  class Op[A](a: A)(implicit ev: A ⇒ Bytes) {
    def `4Bytes`: Layout.`4Bytes`     = Layout.`4Bytes`(ev(a))
    def `8Bytes`: Layout.`8Bytes`     = Layout.`8Bytes`(ev(a))
    def `32Bytes`: Layout.`32Bytes`   = Layout.`32Bytes`(ev(a))
    def `24Bytes`: Layout.`24Bytes`   = Layout.`24Bytes`(ev(a))
    def `60Bytes`: Layout.`60Bytes`   = Layout.`60Bytes`(ev(a))
    def `64Bytes`: Layout.`64Bytes`   = Layout.`64Bytes`(ev(a))
    def `100Bytes`: Layout.`100Bytes` = Layout.`100Bytes`(ev(a))
  }
  implicit def toOp[A](a: A)(implicit ev: A ⇒ Bytes) = new Op[A](a)
  // instances
  implicit val long2BytesInst: Long ⇒ Bytes  = x ⇒ longToBytes(x)
  implicit val int2BytesInst: Int ⇒ Bytes    = x ⇒ intToBytes(x)
  implicit val str2BytesInst: String ⇒ Bytes = x ⇒ x.getBytes("utf-8")
}

trait FillerFileLayout { self ⇒
  import Layout._
  def magic: `4Bytes` = `4Bytes`(FillerFileLayout.HEAD_MAGIC)
  val blockSize: `8Bytes`
  val tailPosition: `8Bytes`
  val version: `4Bytes`
  val versionPos: `8Bytes`

  def head: `32Bytes` = `32Bytes`(
    (magic ++ blockSize ++ tailPosition ++ version ++ versionPos).bytes
  )

  def tailPosition(p: Long): FillerFileLayout = new FillerFileLayout {
    val blockSize: _root_.ufs3.interpreter.layout.Layout.`8Bytes`    = self.blockSize
    val tailPosition: _root_.ufs3.interpreter.layout.Layout.`8Bytes` = p.`8Bytes`
    val version: _root_.ufs3.interpreter.layout.Layout.`4Bytes`      = self.version
    val versionPos: _root_.ufs3.interpreter.layout.Layout.`8Bytes`   = self.versionPos
  }

  def version(v: Int): FillerFileLayout = new FillerFileLayout {
    import Layout._
    val tailPosition: _root_.ufs3.interpreter.layout.Layout.`8Bytes` = self.tailPosition
    val version: _root_.ufs3.interpreter.layout.Layout.`4Bytes` = v.`4Bytes`
    val blockSize: _root_.ufs3.interpreter.layout.Layout.`8Bytes` = self.blockSize
    val versionPos: _root_.ufs3.interpreter.layout.Layout.`8Bytes` = self.versionPos
  }

  def versionPos(p: Long): FillerFileLayout = new FillerFileLayout {
    import Layout._
    val tailPosition: _root_.ufs3.interpreter.layout.Layout.`8Bytes` = self.tailPosition
    val version: _root_.ufs3.interpreter.layout.Layout.`4Bytes` = self.version
    val blockSize: _root_.ufs3.interpreter.layout.Layout.`8Bytes` = self.blockSize
    val versionPos: _root_.ufs3.interpreter.layout.Layout.`8Bytes` = p.`8Bytes`
  }
}

object FillerFileLayout {
  val HEAD_MAGIC: Array[Byte] = "FILL".getBytes("iso8859_1")
  val HEAD_SIZE               = 32L
  def apply(
      _blockSize: Long,
      _tailPosition: Long = HEAD_SIZE,
      _version: Int = 0,
      _versionPos: Long = HEAD_SIZE
  ): FillerFileLayout =
    new FillerFileLayout {
      import Layout._
      // magic is constant

      val blockSize: Layout.Long_Bytes    = _blockSize.`8Bytes`
      val tailPosition: Layout.Long_Bytes = _tailPosition.`8Bytes`
      val version: Layout.Int_Bytes       = _version.`4Bytes`
      val versionPos: Layout.Long_Bytes   = _versionPos.`8Bytes`
    }

  import Layout._
  def resolveBytes(bytes: Bytes): FillerFileLayout = {
    require(bytes.length == HEAD_SIZE, s"filler file header length should be $HEAD_SIZE")
    val magicBytes = bytes.take(4)
    require(magicBytes sameElements HEAD_MAGIC, "filler file magic is not correct")
    val _blockSize       = bytes.slice(4, 12)
    val _tailPosition    = bytes.slice(12, 20)
    val _version         = bytes.slice(20, 24)
    val _versionPosition = bytes.slice(24, 32)

    new FillerFileLayout {
      val tailPosition: _root_.ufs3.interpreter.layout.Layout.`8Bytes` = _tailPosition.`8Bytes`
      val version: _root_.ufs3.interpreter.layout.Layout.`4Bytes`      = _version.`4Bytes`
      val blockSize: _root_.ufs3.interpreter.layout.Layout.`8Bytes`    = _blockSize.`8Bytes`
      val versionPos: _root_.ufs3.interpreter.layout.Layout.`8Bytes`   = _versionPosition.`8Bytes`
    }
  }
}

trait FildexFileLayout {outter ⇒
  import Layout._
  def magic: `4Bytes` = `4Bytes`(FildexFileLayout.HEAD_MAGIC)
  val blockSize: `8Bytes`
  val tailPosition: `8Bytes`
  val version: `4Bytes`

  def head: `24Bytes` = `24Bytes`(
    (magic ++ blockSize ++ tailPosition ++ version).bytes
  )

  def version(v: Int): FildexFileLayout = new FildexFileLayout {
    val tailPosition: _root_.ufs3.interpreter.layout.Layout.`8Bytes` = outter.tailPosition
    val version: _root_.ufs3.interpreter.layout.Layout.`4Bytes` = v.`4Bytes`
    val blockSize: _root_.ufs3.interpreter.layout.Layout.`8Bytes` = outter.blockSize
  }
  def tailPosition(p: Long): FildexFileLayout = new FildexFileLayout {
    val tailPosition: _root_.ufs3.interpreter.layout.Layout.`8Bytes` = p.`8Bytes`
    val version: _root_.ufs3.interpreter.layout.Layout.`4Bytes` = outter.version
    val blockSize: _root_.ufs3.interpreter.layout.Layout.`8Bytes` = outter.blockSize
  }
}

object FildexFileLayout {
  val HEAD_MAGIC: Array[Byte] = "FILD".getBytes("iso8859_1")
  val HEAD_SIZE: Long         = 24
  import Layout._

  def apply(_blockSize: Long, _tailPosition: Long = HEAD_SIZE, _version: Int = 0): FildexFileLayout =
    new FildexFileLayout {
      import Layout._
      val tailPosition: Layout.`8Bytes` = _tailPosition.`8Bytes`
      val version: Layout.`4Bytes`      = _version.`4Bytes`
      val blockSize: Layout.`8Bytes`    = _blockSize.`8Bytes`
    }

  def resolveBytes(bytes: Bytes): FildexFileLayout = {
    require(bytes.length == HEAD_SIZE, s"fildex file header length should be $HEAD_SIZE")
    val magicBytes = bytes.take(4)
    require(magicBytes sameElements HEAD_MAGIC, "fildex file magic is not correct")

    val _blockSize    = bytes.slice(4, 12)
    val _tailPosition = bytes.slice(12, 20)
    val _version      = bytes.slice(20, 24)
    new FildexFileLayout {
      val tailPosition: _root_.ufs3.interpreter.layout.Layout.`8Bytes` = _tailPosition.`8Bytes`
      val version: _root_.ufs3.interpreter.layout.Layout.`4Bytes`      = _version.`4Bytes`
      val blockSize: _root_.ufs3.interpreter.layout.Layout.`8Bytes`    = _blockSize.`8Bytes`
    }
  }
}

object IdxLayout {
  import Layout._
  val SIZE: Long = 48 // key is 32, startPoint 8, end point 8
  def resolveBytes(bytes: Array[Byte]): Idx = {
    require(bytes.length == SIZE, s"fildex index key item should be $SIZE Bytes")
    val key = new String(bytes.take(32), "utf-8")
    val startPoint = bytes.slice(32, 40).`8Bytes`.longValue
    val endPoint = bytes.slice(40, 48).`8Bytes`.longValue
    Idx(key, startPoint, endPoint)
  }
}
