package store

import java.io.{File, FileNotFoundException, RandomAccessFile}
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock}
import java.util.concurrent.{ConcurrentHashMap, Semaphore}
import java.util.concurrent.atomic.AtomicLong

import cats.{Id, ~>}
import store.Response._
import ufs3.kernel.store.FileMode.{ReadOnly, ReadWrite}
import ufs3.kernel.store.Size._
import ufs3.kernel.store.Store._
import ufs3.kernel.store._

/**
  * Created by songwenchao on 2017/7/4.
  */
object StoreInterpreter extends App {

  private val existedMap = new ConcurrentHashMap[String, Boolean]
  private val fillerMap  = new ConcurrentHashMap[String, Filler]
  private val lockMap    = new ConcurrentHashMap[String, FileLock]
  private val statusMap  = new ConcurrentHashMap[String, Boolean]

  private lazy val MAGIC: Array[Byte] = "RRAW".getBytes("iso-8859-1")

  private lazy val poolSize          = 20
  @volatile private var fillerVector = Vector.empty[Filler]
  private lazy val fileSemaphore     = new Semaphore(1)

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
      case Read(filler, size, bufferSize) ⇒
        safeResponse[ReadData](read(filler, size.sizeInByte, bufferSize))
      case Write(filler, data) ⇒
        safeResponse[Unit](write(filler, data.content))
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
      // TODO: wait to implement
      //      case Writable(filler) ⇒
      //       TODO: wait to implement
      //      case Readable(filler) ⇒
    }
  }

  private def existed(path: String): Boolean = {
    if (existedMap.get(path)) true
    else {
      val file   = new File(path)
      val result = file.exists() && file.isFile
      existedMap.putIfAbsent(path, result)
      result
    }
  }

  private def isLegal(filler: Filler): Boolean = {
    if (filler != null && filler.underlying != null) {
      val channel     = filler.underlying.getChannel
      val magicBuffer = channel.map(FileChannel.MapMode.READ_ONLY, 0, 4)
      val magicBytes  = new Array[Byte](4)
      magicBuffer.get(magicBytes)
      val sizeBuffer = channel.map(FileChannel.MapMode.READ_ONLY, 4, 8)
      val size       = sizeBuffer.getLong()
      val tailBuffer = channel.map(FileChannel.MapMode.READ_ONLY, 20, filler.underlying.length() - 20)
      val realSize   = tailBuffer.limit()
      (MAGIC sameElements magicBytes) && (size == realSize)
    } else
      throw new IllegalArgumentException("the file to judge whether legal can not be null")
  }

  private def create(filePath: String, size: Long): Filler = {
    val file = new File(filePath + ".bak")
    file.createNewFile()
    val finalFile = new File(filePath)
    if (!finalFile.exists()) {
      file.renameTo(finalFile)
      val filler = Filler(Path(filePath), size.B, ReadWrite)
      val raf    = filler.underlying
      raf.setLength(size + 20)
      raf.write(MAGIC)
      raf.writeLong(size)
      raf.writeLong(0)
      existedMap.putIfAbsent(filePath, true)
      filler
    } else {
      file.delete()
      throw new IllegalArgumentException(s"the file $filePath already existed ")
    }
  }

  private def delete(path: String): Unit = {
    val file = new File(path)
    if (file.exists()) {
      file.delete()
      ()
    } else ()
  }

  private def getFillerInstance(path: String, mode: FileMode): Filler = {
    fileSemaphore.acquire()
    val fileFiller = if (fillerVector.isEmpty || fillerVector.size <= poolSize) {
      val filler = openFile(path, mode)
      fillerVector = fillerVector :+ filler
      filler
    } else {
      val filler = fillerVector.head
      fillerVector = fillerVector.tail :+ filler
      filler
    }
    fileSemaphore.release()
    fileFiller
  }

  private def openFile(filePath: String, mode: FileMode): Filler = {
    if (existed(filePath)) {
      val raf = new RandomAccessFile(filePath, mode.mode)
      val sizeBuffer =
        raf.getChannel.map(FileChannel.MapMode.READ_ONLY, 4, 8)
      val size   = sizeBuffer.getLong()
      val filler = Filler(Path(filePath), size.B, mode)
      filler
    } else throw new FileNotFoundException(filePath)
  }

  private def open(filePath: String, mode: FileMode): Filler = {
    mode match {
      case ReadOnly ⇒ getFillerInstance(filePath, mode)
      case ReadWrite ⇒
        fillerMap.get(filePath) match {
          case filler: Filler ⇒ filler
          case null ⇒
            val filler = openFile(filePath, mode)
            fillerMap.putIfAbsent(filePath, filler)
            filler
        }
    }
  }

  private def close(filler: Filler): Unit = {
    if (filler != null && filler.underlying != null) {
      val filePath = filler.path.file.value.getPath
      fillerMap.get(filePath) match {
        case fileFiller: Filler ⇒
          fillerMap.remove(filePath)
          lockMap.remove(filePath)
          fileFiller.underlying.close()
        case null ⇒ filler.underlying.close()
      }
    } else
      throw new IllegalArgumentException("the file to close can not be null")
  }

  private def read(filler: Filler, size: Long, bufferSize: Long): ReadData = {
    if (filler != null && filler.underlying != null) {
      def readData(totalNumber: Long, numberRef: AtomicLong, currentPosition: Long, totalSize: Long): ByteBuffer = {
        val tag      = numberRef.getAndDecrement()
        val readSize = if (tag > 1) bufferSize else totalSize - (totalNumber - 1) * bufferSize
        filler.underlying.getChannel
          .map(FileChannel.MapMode.READ_ONLY, currentPosition, readSize)
      }

      ReadData(readData, size.B, bufferSize.B, Position(filler.underlying.getChannel.position()))
    } else
      throw new IllegalArgumentException("the file to read data can not be null")
  }

  private def write(filler: Filler, buffer: ByteBuffer): Unit = {
    if (filler != null && filler.underlying != null) {
      statusMap.putIfAbsent(filler.path.file.value.getPath, true)
      val channel      = filler.underlying.getChannel
      val tailBuffer   = channel.map(FileChannel.MapMode.READ_WRITE, 12, 8)
      val tailPosition = tailBuffer.getLong()
      val remainSize   = filler.blockSize.sizeInByte - tailPosition
      val size =
        if (buffer.position() != 0) buffer.flip().limit() else buffer.limit()
      if (remainSize >= 0 && remainSize - size >= 0 && size > 0) {
        val mappedBuffer =
          channel.map(FileChannel.MapMode.READ_WRITE, tailPosition + 20, size.toLong)
        mappedBuffer.put(buffer)
        buffer.clear()
        val newPosition = tailPosition + size
        tailBuffer.clear()
        tailBuffer.putLong(newPosition)
        statusMap.replace(filler.path.file.value.getPath, true, false)
        ()
      } else {
        statusMap.replace(filler.path.file.value.getPath, true, false)
        throw new IllegalArgumentException(s"the data to write to $filler is too large")
      }
    } else
      throw new IllegalArgumentException("the file to be written can not be null")
  }

  private def lock(filler: Filler): Unit = {
    if (filler != null && filler.underlying != null) {
      val path = filler.path.file.value.getPath
      lockMap.get(path) match {
        case _: FileLock ⇒ throw new IllegalAccessException(s"the file $filler is locked by current APP")
        case null ⇒
          val lock = filler.underlying.getChannel.tryLock()
          if (lock != null) {
            lockMap.putIfAbsent(path, lock)
            ()
          } else throw new IllegalAccessException(s"the file $filler is locked by another APP")
      }
    } else throw new IllegalArgumentException(s"the file $filler to lock can not be null")
  }

  private def unlock(filler: Filler): Unit = {
    if (filler != null && filler.underlying != null) {
      val path = filler.path.file.value.getPath
      lockMap.get(path) match {
        case lock: FileLock ⇒
          lock.release()
          lockMap.remove(path)
          ()
        case null ⇒
      }
    } else throw new IllegalArgumentException(s"the file $filler to unlock can not be null")
  }

  private def freeSpace(filler: Filler): Size = {
    if (filler != null && filler.underlying != null) {
      val tailBuffer =
        filler.underlying.getChannel.map(FileChannel.MapMode.READ_ONLY, 12, 8)
      val usedSize = tailBuffer.getLong()
      (filler.blockSize.sizeInByte - usedSize).B
    } else throw new IllegalArgumentException(s"the file $filler to get free space can not be null")
  }

  private def isWriting(filler: Filler): Boolean = {
    if (filler != null && filler.underlying != null) statusMap.get(filler.path.file.value.getPath)
    else throw new IllegalArgumentException(s"the file $filler to judge write status can not be null")
  }

  private def seekTo(filler: Filler, position: Position): Unit = {
    if (filler != null && filler.underlying != null) filler.underlying.seek(20 + position.value)
    else throw new IllegalArgumentException("the file to seek position can not be null")
  }

  //  val filler = create("/tmp/c.dat", 1024 * 1024)
  //  val buffer = ByteBuffer.allocate(14)
  //  buffer.put("hello,world".getBytes("utf-8"))
  //  write(filler, buffer)
  //  buffer.put("weihui jinrong".getBytes("utf-8"))
  //  write(filler,buffer)
  //  for (i ← 1 to 5) {
  //    new Thread(
  //      new Runnable {
  //        override def run(): Unit = {
  //          val filler = open("/tmp/c.dat", ReadWrite)
  //          seekTo(filler, Position(0))
  //          val data = read(filler, 11)
  //          while (data.hasRemaining) {
  //            val world = data.next
  //            val array = new Array[Byte](world.limit())
  //            world.get(array)
  //            println(Thread.currentThread().getName + "\t" + new String(array, "utf-8"))
  //          }
  //        }
  //      },
  //      s"Thread[$i]"
  //    ).start()
  //  }
}
