package store

import java.io.{File, FileNotFoundException, RandomAccessFile}
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock}
import java.util.concurrent.Semaphore

import cats.{Id, ~>}
import ufs3.kernel.store.Filler.FileFiller
import ufs3.kernel.store._
import ufs3.kernel.store.Store._
import Size._
import ufs3.kernel.store.FileMode.{ReadOnly, ReadWrite}

/**
  * Created by songwenchao on 2017/7/4.
  */
object StoreInterpreter {

  @volatile private var existedMap = Map.empty[String, Boolean]
  @volatile private var fillerMap = Map.empty[String, Filler]
  @volatile private var lockMap = Map.empty[String, FileLock]

  private lazy val fillerSemaphore = new Semaphore(1)
  private lazy val existedSemaphore = new Semaphore(1)

  private lazy val MAGIC: Array[Byte] = "RRAW".getBytes("iso-8859-1")

  lazy val interpreter: Store ~> Id = new (Store ~> Id) {

    override def apply[A](fa: Store[A]): Id[A] = fa match {
      case Existed(path) ⇒
        safeResponse[Boolean](existed(path.file.value.getPath))
      case IsLegal(filler) ⇒
        safeResponse[Boolean](isLegal(filler))
      case Create(path, size) ⇒
        safeResponse[Filler](create(path.file.value.getPath, size.sizeInByte))
      case Delete(path) ⇒
        safeResponse[Unit](delete(path.file.value.getPath))
      case Open(path, mode) ⇒
        safeResponse[Filler](open(path.file.value.getPath, mode))
      case Close(filler) ⇒
        safeResponse[Unit](close(filler))
      case Read(filler, size) ⇒
        safeResponse[Data](read(filler, size.sizeInByte))
      case Write(filler, data) ⇒
        safeResponse(write(filler, data.content))
      case Lock(filler) ⇒
        safeResponse[Unit](lock(filler))
      case UnLock(filler) ⇒
        safeResponse[Unit](unlock(filler))
      case FreeSpace(filler) ⇒
        safeResponse[Size](freeSpace(filler))
      case IsWriting(filler) ⇒
        safeResponse[Boolean](isWriting(filler))
      case SeekTo(filler, position) ⇒
        safeResponse[Unit](seekTo(filler, position))
    }
  }

  def existed(path: String): Boolean = {
    existedSemaphore.acquire()
    val existed = existedMap.get(path) match {
      case Some(result) ⇒ result
      case None ⇒
        val file = new File(path)
        val result = file.exists() && file.isFile
        existedMap += (path → result)
        result
    }
    existedSemaphore.release()
    existed
  }

  def isLegal(filler: Filler): Boolean = {
    if (filler != null && filler.underlying != null) {
      val channel = filler.underlying.getChannel
      val magicBuffer = channel.map(FileChannel.MapMode.READ_ONLY, 0, 4)
      val magicBytes = new Array[Byte](4)
      magicBuffer.get(magicBytes)
      val sizeBuffer = channel.map(FileChannel.MapMode.READ_ONLY, 4, 8)
      val size = sizeBuffer.getLong()
      val tailBuffer = channel.map(FileChannel.MapMode.READ_ONLY,
                                   20,
                                   filler.underlying.length() - 20)
      val realSize = tailBuffer.limit()
      (MAGIC sameElements magicBytes) && (size == realSize)
    } else
      throw new IllegalArgumentException(
        "the file to judge whether legal can not be null")
  }

  def create(filePath: String, size: Long): Filler = {
    fillerSemaphore.acquire()
    val file = new File(filePath + ".bak")
    file.createNewFile()
    val finalFile = new File(filePath)
    if (!finalFile.exists()) {
      file.renameTo(finalFile)
      val raf = new RandomAccessFile(filePath, "rw")
      raf.setLength(20 + size)
      raf.write(MAGIC)
      raf.writeLong(size)
      raf.writeLong(0)
      val filler = new FileFiller {
        override def underlying: RandomAccessFile = raf
        override def blockSize: Size = size.B
        override def path: Path = Path(filePath)
      }
      existedMap += (filePath → true)
      fillerSemaphore.release()
      filler
    } else {
      file.delete()
      fillerSemaphore.release()
      throw new IllegalArgumentException(
        s"the file $filePath already existed ")
    }
  }

  def delete(path: String): Unit = {
    val file = new File(path)
    if (file.exists()) {
      file.delete()
      ()
    } else ()
  }

  def open(filePath: String, mode: FileMode): Filler = {
    def openFile(): Filler = {
      if (existed(filePath)) {
        val raf = new RandomAccessFile(filePath, mode.mode)
        val sizeBuffer =
          raf.getChannel.map(FileChannel.MapMode.READ_ONLY, 4, 8)
        val size = sizeBuffer.getLong()
        new FileFiller {
          override def underlying: RandomAccessFile = raf
          override def blockSize: Size = size.B
          override def path: Path = Path(filePath)
        }
      } else throw new FileNotFoundException(filePath)
    }

    mode match {
      case ReadOnly ⇒ openFile()
      case ReadWrite ⇒
        fillerSemaphore.acquire()
        val fileFiller = fillerMap.get(filePath) match {
          case Some(filler) ⇒ filler
          case None ⇒
            val filler = openFile()
            fillerMap += (filePath → filler)
            filler
        }
        fillerSemaphore.release()
        fileFiller
    }
  }

  def close(filler: Filler): Unit = {
    if (filler != null && filler.underlying != null) {
      fillerSemaphore.acquire()
      val filePath = filler.path.file.value.getPath
      fillerMap.get(filePath) match {
        case Some(fileFiller) ⇒
          fillerMap -= filePath
          lockMap -= filePath
          fileFiller.underlying.close()
        case None ⇒ filler.underlying.close()
      }
      fillerSemaphore.release()
    } else
      throw new IllegalArgumentException("the file to close can not be null")
  }

  def read(filler: Filler, size: Long): Data = {
    if (filler != null && filler.underlying != null) {
      val mappedBuffer = filler.underlying.getChannel.map(
        FileChannel.MapMode.READ_ONLY,
        filler.underlying.getChannel.position(),
        size)
      Data(mappedBuffer)
    } else
      throw new IllegalArgumentException(
        "the file to read data can not be null")
  }

  def write(filler: Filler, buffer: ByteBuffer): Unit = {
    if (filler != null && filler.underlying != null) {
      val channel = filler.underlying.getChannel
      val tailBuffer = channel.map(FileChannel.MapMode.READ_WRITE, 12, 8)
      val tailPosition = tailBuffer.getLong()
      val remainSize = filler.blockSize.sizeInByte - tailPosition
      val size =
        if (buffer.position() != 0) buffer.flip().limit() else buffer.limit()
      if (remainSize >= 0 && remainSize - size >= 0 && size > 0) {
        val mappedBuffer =
          channel.map(FileChannel.MapMode.READ_WRITE,
                      tailPosition + 20,
                      size.toLong)
        mappedBuffer.put(buffer)
        buffer.clear()
        val newPosition = tailPosition + size
        tailBuffer.clear()
        tailBuffer.putLong(newPosition)
        ()
      } else
        throw new IllegalArgumentException(
          s"the fdata to write to $filler is too large")
    } else
      throw new IllegalArgumentException(
        "the file to be written can not be null")
  }

  def lock(filler: Filler): Unit = {
    if (filler != null && filler.underlying != null) {
      fillerSemaphore.acquire()
      val path = filler.path.file.value.getPath
      lockMap.get(path) match {
        case Some(_) ⇒
          fillerSemaphore.release()
          throw new IllegalAccessException(
            s"the file $filler is locked by current APP")
        case None ⇒
          val lock = filler.underlying.getChannel.tryLock()
          if (lock != null) {
            lockMap += (path → lock)
            fillerSemaphore.release()
          } else {
            fillerSemaphore.release()
            throw new IllegalAccessException(
              s"the file $filler is locked by another APP")
          }
      }
    } else
      throw new IllegalArgumentException(
        s"the file $filler to lock can not be null")
  }

  def unlock(filler: Filler): Unit = {
    if (filler != null && filler.underlying != null) {
      fillerSemaphore.acquire()
      val path = filler.path.file.value.getPath
      lockMap.get(path) match {
        case Some(lock) ⇒
          lock.release()
          lockMap -= path
        case None ⇒
      }
      fillerSemaphore.release()
    } else
      throw new IllegalArgumentException(
        s"the file $filler to unlock can not be null")
  }

  def freeSpace(filler: Filler): Size = {
    if (filler != null && filler.underlying != null) {
      val tailBuffer =
        filler.underlying.getChannel.map(FileChannel.MapMode.READ_ONLY, 12, 8)
      val usedSize = tailBuffer.getLong()
      (filler.blockSize.sizeInByte - usedSize).B
    } else
      throw new IllegalArgumentException(
        s"the file $filler to get free space can not be null")
  }

  def isWriting(filler: Filler): Boolean = {
    if (filler != null && filler.underlying != null) {
      fillerSemaphore.acquire()
      val path = filler.path.file.value.getPath
      val status = lockMap.get(path) match {
        case Some(_) ⇒ true
        case None ⇒
          val lock = filler.underlying.getChannel.tryLock()
          if (lock == null) true else false
      }
      fillerSemaphore.release()
      status
    } else
      throw new IllegalArgumentException(
        s"the file $filler to judge write status can not be null")
  }

  def seekTo(filler: Filler, position: Position): Unit = {
    if (filler != null && filler.underlying != null) {
      filler.underlying.seek(20 + position.value)
    } else
      throw new IllegalArgumentException(
        "the file to seek position can not be null")
  }

  def safeResponse[A](a: ⇒ A): Either[Throwable, A] =
    try {
      Right(a)
    } catch {
      case t: Throwable ⇒ Left(t)
    }

}
