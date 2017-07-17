package ufs3
package interpreter
package block

import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer
import java.nio.channels.FileLock
import java.util.concurrent.ConcurrentHashMap

import cats.data.Kleisli
import cats.effect.IO
import ufs3.kernel.block.Block
import ufs3.kernel.block.Block.{BlockFile, FileMode}

/**
  * Created by songwenchao on 2017/7/17.
  */
trait BlockInterpreter extends Block.Handler[Kleisli[IO, Unit, ?]] {

  private[this] lazy val lockMap = new ConcurrentHashMap[BlockFile, FileLock]

  override protected[this] def open(path: Block.Path, mode: Block.FileMode): Kleisli[IO, Unit, Block.BlockFile] =
    Kleisli { _ ⇒
      IO.pure(BlockFile(path, mode).value)
    }

  override protected[this] def close(bf: Block.BlockFile): Kleisli[IO, Unit, Unit] = Kleisli { _ ⇒
    IO.pure(bf.close())
  }

  override protected[this] def create(path: Block.Path, size: Block.Size): Kleisli[IO, Unit, Block.BlockFile] =
    Kleisli { _ ⇒
      IO.pure {
        val filePath = path.file.value.getPath
        val raf      = new RandomAccessFile(filePath, "rw")
        raf.setLength(size.sizeInByte)
        BlockFile(path, FileMode.ReadWrite).value
      }
    }

  override protected[this] def delete(path: Block.Path): Kleisli[IO, Unit, Unit] = Kleisli { _ ⇒
    IO.pure {
      new File(path.file.value.getPath).delete()
      ()
    }
  }

  override protected[this] def seek(blockFile: Block.BlockFile, pos: Long): Kleisli[IO, Unit, Unit] = Kleisli { _ ⇒
    IO.pure(blockFile.seek(pos))
  }

  override protected[this] def read(blockFile: Block.BlockFile, size: Block.Size): Kleisli[IO, Unit, ByteBuffer] =
    Kleisli { _ ⇒
      IO.pure(blockFile.read(size.sizeInByte))
    }

  override protected[this] def write(blockFile: Block.BlockFile, data: ByteBuffer): Kleisli[IO, Unit, Unit] = Kleisli {
    _ ⇒
      IO.pure {
        val size = if (data.position() == 0) data.limit() else data.flip().limit()
        blockFile.write(data, size.toLong)
      }
  }

  override protected[this] def lock(blockFile: Block.BlockFile): Kleisli[IO, Unit, Boolean] = Kleisli { _ ⇒
    IO.pure {
      val lock = blockFile.lock()
      if (lock == null) false
      else {
        lockMap.putIfAbsent(blockFile, lock)
        true
      }
    }
  }

  override protected[this] def unlock(blockFile: Block.BlockFile): Kleisli[IO, Unit, Unit] = Kleisli { _ ⇒
    IO.pure {
      val lock = lockMap.get(blockFile)
      if (lock != null) lock.release() else ()
    }
  }

  override protected[this] def existed(path: Block.Path): Kleisli[IO, Unit, Boolean] = Kleisli { _ ⇒
    IO.pure(new File(path.file.value.getPath).exists())
  }
}

object BlockInterpreter {
  def apply(): BlockInterpreter = new BlockInterpreter {}
}
