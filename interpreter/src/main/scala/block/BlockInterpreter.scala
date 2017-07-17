package block

import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer
import java.nio.channels.FileLock
import java.util.concurrent.ConcurrentHashMap

import cats.Id
import ufs3.kernel.block.Block
import ufs3.kernel.block.Block.{BlockFile, FileMode}

/**
  * Created by songwenchao on 2017/7/14.
  */
object BlockInterpreter extends Block.Handler[Id] {

  private[this] lazy val lockMap = new ConcurrentHashMap[BlockFile, FileLock]

  override protected[this] def open(path: Block.Path, mode: Block.FileMode): Id[Block.BlockFile] =
    BlockFile(path, mode).value

  override protected[this] def close(bf: Block.BlockFile): Id[Unit] = bf.close()

  override protected[this] def create(path: Block.Path, size: Block.Size): Id[Block.BlockFile] = {
    val filePath = path.file.value.getPath
    val raf      = new RandomAccessFile(filePath, "rw")
    raf.setLength(size.sizeInByte)
    BlockFile(path, FileMode.ReadWrite).value
  }

  override protected[this] def delete(path: Block.Path): Id[Unit] = { new File(path.file.value.getPath).delete(); () }

  override protected[this] def seek(blockFile: Block.BlockFile, pos: Long): Id[Unit] = blockFile.seek(pos)

  override protected[this] def read(blockFile: Block.BlockFile, size: Block.Size): Id[ByteBuffer] =
    blockFile.read(size.sizeInByte)

  override protected[this] def write(blockFile: Block.BlockFile, data: ByteBuffer): Id[Unit] = {
    val size = if (data.position() == 0) data.limit() else data.flip().limit()
    blockFile.write(data, size.toLong)
  }

  override protected[this] def lock(blockFile: Block.BlockFile): Id[Boolean] = {
    val lock = blockFile.lock()
    if (lock == null) false
    else {
      lockMap.putIfAbsent(blockFile, lock)
      true
    }
  }

  override protected[this] def unlock(blockFile: Block.BlockFile): Id[Unit] = {
    val lock = lockMap.get(blockFile)
    if (lock != null) lock.release() else ()
  }

  override protected[this] def existed(path: Block.Path): Id[Boolean] = new File(path.file.value.getPath).exists()
}
