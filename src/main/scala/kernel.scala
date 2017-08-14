package ufs3.kernel
import java.io.File
import java.nio.ByteBuffer

import cats.Eval
import freestyle._
import ufs3.kernel
import cats.implicits._

import scala.language.implicitConversions

object algebras {
  import commons._

  // Block
  @free trait Block {
    def existed(path: Path): FS[Boolean]
    def open(path: Path, mode: FileMode): FS[BlockFile]
    def close(bf: BlockFile): FS[Unit]
    def create(path: Path, size: Size): FS[BlockFile]
    def delete(path: Path): FS[Unit]
    def seek(bf: BlockFile, pos: Long): FS[Unit]
    def read(bf: BlockFile, size: Size): FS[ByteBuffer]
    def write(bf: BlockFile, data: ByteBuffer): FS[Unit]
    def lock(bf: BlockFile): FS[Boolean]
    def unlock(bf: BlockFile): FS[Unit]
  }

  // Filler
  @free trait Filler {
    def init(bf: BlockFile): FS[FillerFile]
    def check(bf: BlockFile): FS[FillerFile]
    def close(ff: FillerFile): FS[Unit]
    def startAppend(ff: FillerFile): FS[Long]
    def endAppend(ff: FillerFile, startPosition: Long, endPosition: Long): FS[FillerFile]
    def freeSpace(ff: FillerFile): FS[Long]
    def isWriting(ff: FillerFile): FS[Boolean]
    def forceToWrite(ff: FillerFile): FS[Unit]
  }

  // Fildex
  @free trait Fildex {
    def init(bf: BlockFile): FS[FildexFile]
    def check(bf: BlockFile, ff: FillerFile): FS[Boolean]
    def repair(bf: BlockFile, ff: FillerFile): FS[FildexFile]
    def load(bf: BlockFile): FS[FildexFile]
    def append(fdf: FildexFile, idx: Idx): FS[FildexFile]
    def close(fdf: FildexFile): FS[Unit]
    def fetchKey(key: String, fdf: FildexFile): FS[Option[Idx]]
    def fetchUuid(uuid: String, fdf: FildexFile): FS[Option[Idx]]
    def query(limit: Int, order: Order, fdf: FildexFile): FS[Vector[Idx]]
    def freeSpace(fdf: FildexFile): FS[Long]
  }

  // SandwichIn
  /*
  @free trait SandwichIn[In] {
    def head(key: String, uuid: String, bodyLength: Long): FS[ByteBuffer]
    def nextBody(in: In): FS[Option[ByteBuffer]]
    def tail(hash: Array[Byte], bodyLength: Long): FS[ByteBuffer]
  }
   */

  // Sandwich Out
  /*
  @free trait SandwichOut[Out] {
    def headSize(): FS[Long]
    def tailSize(): FS[Long]
    def outputHead(bb: ByteBuffer, out: Out): FS[Unit]
    def outputBody(bb: ByteBuffer, out: Out): FS[Unit]
    def outputTail(bb: ByteBuffer, out: Out): FS[Unit]
  }
 */
}

object commons {
  // the path for the block file
  case class Path(p: String) {
    val file: Eval[File]      = Eval.later(new File(p))
    val indexPath: Eval[Path] = file.map((f: File) ⇒ Path(f.getAbsolutePath + ".idx"))
  }

  // XFS-friendly big size file
  private[ufs3] trait BlockFile
  // Filler File
  private[ufs3] trait FillerFile
  // Fildex File
  private[ufs3] trait FildexFile
  // Config
  case class Config(
      fillerBlockPath: Path,
      fillerBlockSize: Size,
      idxBlockSize: Size,
      fillerReadBufferSize: Size
  )

  // Order
  sealed trait Order
  case object Asc  extends Order
  case object Desc extends Order
  object Order {
    def apply(o: String): Order = o match {
      case "desc" ⇒ Desc
      case _      ⇒ Asc
    }
  }

  /**
    * FileMode
    * --------
    * The mode used to Read/Write a file.
    * - ReadOnly: the file opened readonly
    * - ReadWrite: the file opened readable and writable
    */
  sealed trait FileMode {
    def mode: String
    override def toString: String = mode
  }
  object FileMode {
    case object ReadOnly extends FileMode {
      val mode: String = "r"
    }
    case object ReadWrite extends FileMode {
      val mode: String = "rw"
    }
  }

  // Index structure
  final case class Idx(key: String, uuid: String, startPoint: Long, endPoint: Long) {
    require(key.getBytes("utf-8").length == 32, "fildex index key should be 32bit String")
    require(uuid.getBytes("utf-8").length == 32, "fildex index uuid should be 32bit String")

    def byteBuffer: ByteBuffer = {
      val bb = ByteBuffer.allocate(80)
      bb.put(key.getBytes("utf-8"))
      bb.put(uuid.getBytes("utf-8"))
      bb.putLong(startPoint)
      bb.putLong(endPoint)
      bb.flip()
      bb
    }

    def fileLength: Long = endPoint - startPoint
  }

  /**
    * Size
    * ----
    * Wrapper of Long, to identity the file length in bytes
    */
  sealed trait Size {
    def sizeInByte: Long
    def toStringWithUnit(u: String): String = u match {
      case "G" ⇒ "%.2fGiB" format sizeInByte.toDouble / 1024 / 1024 / 1024
      case "M" ⇒ "%.2fMiB" format sizeInByte.toDouble / 1024 / 1024
      case "K" ⇒ "%.2fKiB" format sizeInByte.toDouble / 1024
      case _   ⇒ sizeInByte.toString + "B"
    }
  }

  // size
  object Size {
    sealed trait ToLong[A] {
      def toLong(a: A): Long
    }
    final class SizeOp[A](a: A)(implicit ev: ToLong[A]) {
      def B: Size = new Size {
        override def sizeInByte: Long = ev.toLong(a)
      }
      def KiB: Size = new Size {
        def sizeInByte: Long = ev.toLong(a) * 1024
      }
      def MiB: Size = new Size {
        override def sizeInByte: Long = ev.toLong(a) * 1024 * 1024
      }
      def GiB: Size = new Size {
        def sizeInByte: Long = ev.toLong(a) * 1024 * 1024 * 1024
      }
    }

    implicit def AnyToLong[A]: ToLong[A] = new ToLong[A] {
      override def toLong(a: A): Long = a.toString.toLong
    }

    implicit def toSizeOp[A](a: A)(implicit ev: ToLong[A]): SizeOp[A] = new SizeOp[A](a)
  }

  object Layout {
    type Bytes = Array[Byte]

    abstract class FixedLengthBytes(val len: Int) { outter ⇒
      require(bytes.length == len, s"bytes length should be $len")
      def bytes: Bytes
      def byteBuffer: ByteBuffer = ByteBuffer.wrap(bytes)

      def ++(n: FixedLengthBytes): FixedLengthBytes = new FixedLengthBytes(len + n.len) {
        def bytes: Bytes = outter.bytes ++ n.bytes
      }

      def longValue: Long     = byteBuffer.getLong
      def intValue: Int       = byteBuffer.getInt
      def stringValue: String = new String(bytes, "utf-8")

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
    case class `40Bytes`(bytes: Bytes)  extends FixedLengthBytes(40)
    case class `52Bytes`(bytes: Bytes)  extends FixedLengthBytes(52)
    case class `60Bytes`(bytes: Bytes)  extends FixedLengthBytes(60)
    case class `64Bytes`(bytes: Bytes)  extends FixedLengthBytes(64)
    case class `84Bytes`(bytes: Bytes)  extends FixedLengthBytes(84)
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
      def `40Bytes`: Layout.`40Bytes`   = Layout.`40Bytes`(ev(a))
      def `52Bytes`: Layout.`52Bytes`   = Layout.`52Bytes`(ev(a))
      def `60Bytes`: Layout.`60Bytes`   = Layout.`60Bytes`(ev(a))
      def `64Bytes`: Layout.`64Bytes`   = Layout.`64Bytes`(ev(a))
      def `84Bytes`: Layout.`84Bytes`   = Layout.`84Bytes`(ev(a))
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
      val blockSize: kernel.commons.Layout.`8Bytes`    = self.blockSize
      val tailPosition: kernel.commons.Layout.`8Bytes` = p.`8Bytes`
      val version: kernel.commons.Layout.`4Bytes`      = self.version
      val versionPos: kernel.commons.Layout.`8Bytes`   = self.versionPos
    }

    def version(v: Int): FillerFileLayout = new FillerFileLayout {
      import Layout._
      val tailPosition: kernel.commons.Layout.`8Bytes` = self.tailPosition
      val version: kernel.commons.Layout.`4Bytes`      = v.`4Bytes`
      val blockSize: kernel.commons.Layout.`8Bytes`    = self.blockSize
      val versionPos: kernel.commons.Layout.`8Bytes`   = self.versionPos
    }

    def versionPos(p: Long): FillerFileLayout = new FillerFileLayout {
      import Layout._
      val tailPosition: kernel.commons.Layout.`8Bytes` = self.tailPosition
      val version: kernel.commons.Layout.`4Bytes`      = self.version
      val blockSize: kernel.commons.Layout.`8Bytes`    = self.blockSize
      val versionPos: kernel.commons.Layout.`8Bytes`   = p.`8Bytes`
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
        val tailPosition: kernel.commons.Layout.`8Bytes` = _tailPosition.`8Bytes`
        val version: kernel.commons.Layout.`4Bytes`      = _version.`4Bytes`
        val blockSize: kernel.commons.Layout.`8Bytes`    = _blockSize.`8Bytes`
        val versionPos: kernel.commons.Layout.`8Bytes`   = _versionPosition.`8Bytes`
      }
    }
  }

  trait FildexFileLayout { outter ⇒
    import Layout._
    def magic: `4Bytes` = `4Bytes`(FildexFileLayout.HEAD_MAGIC)
    val blockSize: `8Bytes`
    val tailPosition: `8Bytes`
    val version: `4Bytes`

    def head: `24Bytes` = `24Bytes`(
      (magic ++ blockSize ++ tailPosition ++ version).bytes
    )

    def version(v: Int): FildexFileLayout = new FildexFileLayout {
      val tailPosition: kernel.commons.Layout.`8Bytes` = outter.tailPosition
      val version: kernel.commons.Layout.`4Bytes`      = v.`4Bytes`
      val blockSize: kernel.commons.Layout.`8Bytes`    = outter.blockSize
    }
    def tailPosition(p: Long): FildexFileLayout = new FildexFileLayout {
      val tailPosition: kernel.commons.Layout.`8Bytes` = p.`8Bytes`
      val version: kernel.commons.Layout.`4Bytes`      = outter.version
      val blockSize: kernel.commons.Layout.`8Bytes`    = outter.blockSize
    }
  }

  object FildexFileLayout {
    val HEAD_MAGIC: Array[Byte] = "FILD".getBytes("iso8859_1")
    val HEAD_SIZE: Long         = 24L
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
        val tailPosition: kernel.commons.Layout.`8Bytes` = _tailPosition.`8Bytes`
        val version: kernel.commons.Layout.`4Bytes`      = _version.`4Bytes`
        val blockSize: kernel.commons.Layout.`8Bytes`    = _blockSize.`8Bytes`
      }
    }
  }

  object IdxLayout {
    import Layout._
    val SIZE: Long = 80 // key is 32, uuid is 32, startPoint 8, end point 8
    def resolveBytes(bytes: Array[Byte]): Idx = {
      require(bytes.length == SIZE, s"fildex index key item should be $SIZE Bytes")
      val key        = new String(bytes.take(32), "utf-8")
      val uuid       = new String(bytes.slice(32, 64), "utf-8")
      val startPoint = bytes.slice(64, 72).`8Bytes`.longValue
      val endPoint   = bytes.slice(72, 80).`8Bytes`.longValue
      Idx(key, uuid, startPoint, endPoint)
    }
  }

  trait SandwichHeadLayout { outter ⇒
    import Layout._
    def magic: `4Bytes`      = `4Bytes`(SandwichHeadLayout.HEAD_MAGIC)
    val createTime: `8Bytes` = `8Bytes`(System.currentTimeMillis())
    val key: `32Bytes`
    val uuid: `32Bytes`
    val bodyLength: `8Bytes`

    def head: `84Bytes` = `84Bytes`(
      (magic ++ createTime ++ key ++ uuid ++ bodyLength).bytes
    )

    def uuid(bytes: `32Bytes`): SandwichHeadLayout = new SandwichHeadLayout {
      override val createTime: `8Bytes`              = `8Bytes`(System.currentTimeMillis())
      val bodyLength: kernel.commons.Layout.`8Bytes` = outter.bodyLength
      val uuid: `32Bytes`                            = bytes
      val key: `32Bytes`                             = outter.key
    }

  }

  object SandwichHeadLayout {
    // the size of Sandwich-Head is 52 Bytes
    // [0 - 4): the magic, 'UFS3'
    // [4 - 12): the create time
    // [12 - 44): the key of the file
    // [44 - 52): the body length
    val HEAD_MAGIC: Array[Byte] = "UFS3".getBytes
    val HEAD_LENGTH: Int        = 84
    //the size of sandwich created time millis
    val HEAD_CREATE_TIME_SIZE: Int = 8
    //the size  of sandwich name converted to hex string
    val HEAD_KEY_SIZE: Int = 32
    //the size of sandwich body
    val HEAD_BODY_LENGTH_SIZE: Int = 8

    import Layout._

    def apply(_key: String, _uuid: String, _bodyLength: Long): SandwichHeadLayout = new SandwichHeadLayout {
      val key: Layout.`32Bytes`       = _key.`32Bytes`
      val uuid: `32Bytes`             = _uuid.`32Bytes`
      val bodyLength: Layout.`8Bytes` = _bodyLength.`8Bytes`
    }

    def resolveBytes(bytes: Bytes): SandwichHeadLayout = {
      require(bytes.length == HEAD_LENGTH, s"Sandwich head plus tail size must eq $HEAD_LENGTH ")
      val magicByes = bytes.take(4)
      require(magicByes sameElements HEAD_MAGIC, s"Sandwich head magic must be $HEAD_MAGIC")
      val createTimeBytes = bytes.slice(4, 12)
      val keyBytes        = bytes.slice(12, 44)
      val uuidBytes       = bytes.slice(44, 76)
      val bodyLengthBytes = bytes.slice(76, 84)
      new SandwichHeadLayout {
        override val createTime: `8Bytes` = createTimeBytes.`8Bytes`
        val key: `32Bytes`                = keyBytes.`32Bytes`
        val uuid: `32Bytes`               = uuidBytes.`32Bytes`
        val bodyLength: `8Bytes`          = bodyLengthBytes.`8Bytes`
      }
    }
  }

  trait SandwichTailLayout {
    import Layout._
    def hash: `32Bytes`
    def bodyLength: `8Bytes`

    def tail: `40Bytes` = `40Bytes`(
      (hash ++ bodyLength).bytes
    )
  }

  object SandwichTailLayout {
    // the size of Sandwich-Tail is 40 Bytes
    // [0-32): the hash of body
    // [32-40): the body length
    // the size of Sandwich-Tail is 16 Bytes, the hash of the body
    val TAIL_HASH_SIZE: Int = 32
    //the size of sandwich body stored in tail
    val TAIL_BODY_LENGTH_SIZE: Int = 8
    //the size of sandwich tail
    val TAIL_LENGTH: Int = 40

    import Layout._

    def apply(_hash: Bytes, _bodyLength: Long): SandwichTailLayout = new SandwichTailLayout {
      override def hash: `32Bytes`      = _hash.`32Bytes`
      override def bodyLength: `8Bytes` = _bodyLength.`8Bytes`
    }

    def resolveBytes(bytes: Bytes): SandwichTailLayout = {
      require(bytes.length == TAIL_LENGTH, s"Sandwich tail size must eq $TAIL_LENGTH")
      val hashBytes       = bytes.take(32)
      val bodyLengthBytes = bytes.slice(32, 40)
      new SandwichTailLayout {
        override def hash: `32Bytes`      = hashBytes.`32Bytes`
        override def bodyLength: `8Bytes` = bodyLengthBytes.`8Bytes`
      }
    }
  }

  object SandwichLayout {
    implicit def from(byteBuffer: ByteBuffer): Array[Byte] = {
      if (byteBuffer.position() != 0) byteBuffer.flip()
      val byteArray = new Array[Byte](byteBuffer.limit())
      byteBuffer.get(byteArray)
      byteArray
    }
  }
}
