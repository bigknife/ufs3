package ufs3
package interpreter
package block

import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer
import java.nio.channels.FileLock
import java.util.concurrent.ConcurrentHashMap

import cats.data.Kleisli
import cats.effect.IO
import ufs3.interpreter.block.BlockInterpreter.Config
import ufs3.kernel.block.Block
import ufs3.kernel.block.Block.{BlockFile, FileMode}

/**
  * Created by songwenchao on 2017/7/17.
  */
trait BlockInterpreter extends Block.Handler[Kleisli[IO, Config, ?]] {

  import RandomAccessBlockFile._

  private[this] lazy val lockMap = new ConcurrentHashMap[BlockFile, FileLock]

  override protected[this] def open(path: Block.Path, mode: Block.FileMode): Kleisli[IO, Config, Block.BlockFile] =
    Kleisli { config ⇒
      IO(RandomAccessBlockFile(path, mode).value)
    }

  override protected[this] def close(bf: Block.BlockFile): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    IO(bf.close())
  }

  override protected[this] def create(path: Block.Path, size: Block.Size): Kleisli[IO, Config, Block.BlockFile] =
    Kleisli { config ⇒
      IO {
        /*
        val filePath = path.file.value.getPath
        val raf      = new RandomAccessFile(filePath, "rw")
        raf.setLength(size.sizeInByte)
        raf.close()
        */
        RandomAccessBlockFile(path, FileMode.ReadWrite).map(x ⇒ x.size(size.sizeInByte)).value
      }
    }

  override protected[this] def delete(path: Block.Path): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    IO {
      new File(path.file.value.getPath).delete()
      ()
    }
  }

  override protected[this] def seek(blockFile: Block.BlockFile, pos: Long): Kleisli[IO, Config, Unit] = Kleisli {
    config ⇒
      IO(blockFile.seek(pos))
  }

  override protected[this] def read(blockFile: Block.BlockFile, size: Block.Size): Kleisli[IO, Config, ByteBuffer] =
    Kleisli { config ⇒
      IO(blockFile.read(size.sizeInByte))
    }

  override protected[this] def write(blockFile: Block.BlockFile, data: ByteBuffer): Kleisli[IO, Config, Unit] =
    Kleisli { config ⇒
      IO {
        val size = if (data.position() == 0) data.limit() else data.flip().limit()
        blockFile.write(data, size)
      }
    }

  override protected[this] def lock(blockFile: Block.BlockFile): Kleisli[IO, Config, Boolean] = Kleisli { config ⇒
    IO {
      val lock = blockFile.lock()
      if (lock == null) false
      else {
        lockMap.putIfAbsent(blockFile, lock)
        true
      }
    }
  }

  override protected[this] def unlock(blockFile: Block.BlockFile): Kleisli[IO, Config, Unit] = Kleisli { config ⇒
    IO {
      val lock = lockMap.get(blockFile)
      if (lock != null) lock.release() else ()
    }
  }

  override protected[this] def existed(path: Block.Path): Kleisli[IO, Config, Boolean] = Kleisli { config ⇒
    IO(new File(path.file.value.getPath).exists())
  }
}

object BlockInterpreter {

  trait Config

  def config(): Config = new Config {}

  def apply(): BlockInterpreter = new BlockInterpreter {}
}
