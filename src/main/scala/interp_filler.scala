package ufs3.interp
import ufs3.kernel.algebras.Filler
import ufs3.kernel.commons._
import java.util.concurrent.atomic.AtomicInteger
import scala.language.implicitConversions
import ufs3.interp.block.{BlockFileBasedFile, RandomAccessBlockFile}

object filler {

  import commons._
  import RandomAccessBlockFile._

  val atomWriting = new AtomicInteger(0)

  implicit val fillerInterp: Filler.Handler[Stack] = new Filler.Handler[Stack] {
    protected[this] def init(bf: BlockFile): Stack[FillerFile] = Stack.configless[FillerFile] {
      val layout = FillerFileLayout(bf.size())
      RandomFillerFile(layout, underlying = bf).init()
    }

    protected[this] def check(bf: BlockFile): Stack[FillerFile] = Stack.configless[FillerFile] {
      require(bf.size() >= FillerFileLayout.HEAD_SIZE,
              s"the block file length should be greater than ${FillerFileLayout.HEAD_SIZE}")

      val headBytes = {
        bf.seek(0)
        val bb    = bf.read(FillerFileLayout.HEAD_SIZE)
        val bytes = new Array[Byte](FillerFileLayout.HEAD_SIZE.toInt)
        bb.get(bytes)
        bytes
      }
      // the magic check is in the `resoveBytes`
      val layout = FillerFileLayout.resolveBytes(headBytes)
      require(bf.size() == layout.blockSize.longValue, s"the block file length should eq ${layout.blockSize.longValue}")
      RandomFillerFile(layout = layout, underlying = bf)
    }

    protected[this] def close(ff: FillerFile): Stack[Unit] = Stack.configless[Unit](())

    protected[this] def startAppend(ff: FillerFile): Stack[Long] = Stack.configless[Long] {
      import RandomFillerFile._
      require(!ff.isFull, "the filler file is full")

      atomWriting.incrementAndGet()
      ff.tailPos
    }

    protected[this] def endAppend(ff: FillerFile, startPosition: Long, endPosition: Long): Stack[FillerFile] =
      Stack.configless[FillerFile] {
        import RandomFillerFile._
        atomWriting.decrementAndGet()
        RandomFillerFile.from(ff).tailPos(endPosition).version(ff.version + 1).versionPos(startPosition).refreshHead()
      }

    protected[this] def freeSpace(ff: FillerFile): Stack[Long] = Stack.configless[Long] {
      import RandomFillerFile._
      ff.freeSpace
    }

    protected[this] def isWriting(ff: FillerFile): Stack[Boolean] = Stack.configless[Boolean](atomWriting.get != 0)

    protected[this] def forceToWrite(ff: FillerFile): Stack[Unit] = Stack.configless[Unit](atomWriting.set(0))
  }

  final class RandomFillerFile(private val layout: FillerFileLayout, val underlying: RandomAccessBlockFile)
      extends FillerFile
      with BlockFileBasedFile {

    def init(): RandomFillerFile = {
      refreshHead()
      this
    }

    // current tail position
    def tailPos: Long = layout.tailPosition.longValue

    // free space
    def freeSpace: Long = underlying.size() - tailPos

    def isFull: Boolean = layout.tailPosition.longValue >= underlying.size()

    // set current tail position
    def tailPos(pos: Long): RandomFillerFile = {
      val newLayout = layout.tailPosition(pos)
      RandomFillerFile(newLayout, underlying)
    }

    // refresh head in the file
    def refreshHead(): RandomFillerFile = {
      seek(0)
      write(layout.head.byteBuffer)
      this
    }

    // get current version
    def version: Int = layout.version.intValue

    def version(newVewsion: Int): RandomFillerFile = {
      val newLayout = layout.version(newVewsion)
      RandomFillerFile(newLayout, underlying)
    }

    def versionPos(p: Long): RandomFillerFile = {
      val newLayout = layout.versionPos(p)
      RandomFillerFile(newLayout, underlying)
    }

  }

  object RandomFillerFile {

    def apply(layout: FillerFileLayout, underlying: RandomAccessBlockFile): RandomFillerFile =
      new RandomFillerFile(layout, underlying)

    def magicMatched(bytes: Array[Byte]): Boolean = bytes sameElements FillerFileLayout.HEAD_MAGIC

    implicit def from(ff: FillerFile): RandomFillerFile = ff.asInstanceOf[RandomFillerFile]

  }

}
