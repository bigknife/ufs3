package ufs3
package interpreter
package block

import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer
import java.nio.channels.FileLock
import java.util.concurrent.ConcurrentHashMap

import cats.data.Kleisli
import cats.effect.IO
import sop.Resp
import ufs3.interpreter.block.BlockInterpreter.Config
import ufs3.kernel.block.Block
import ufs3.kernel.block.Block.{BlockFile, FileMode}

/**
  * Created by songwenchao on 2017/7/17.
  */
trait BlockInterpreter extends Block.Handler[Kleisli[IO, Config, ?]] {

  import RandomAccessBlockFile._

  private[this] lazy val lockMap = new ConcurrentHashMap[BlockFile, FileLock]

  override protected[this] def open(path: Block.Path,
                                    mode: Block.FileMode): Kleisli[IO, Config, Resp[Block.BlockFile]] =
    Kleisli { config ⇒
      IO {
        RandomAccessBlockFile(path, mode).value
      }.attempt
    }

  override protected[this] def close(bf: Block.BlockFile): Kleisli[IO, Config, Resp[Unit]] = Kleisli { config ⇒
    IO { bf.close() }.attempt
  }

  override protected[this] def create(path: Block.Path, size: Block.Size): Kleisli[IO, Config, Resp[Block.BlockFile]] =
    Kleisli { config ⇒
      IO {
        RandomAccessBlockFile(path, FileMode.ReadWrite).map(x ⇒ x.size(size.sizeInByte)).value
      }.attempt
    }

  override protected[this] def delete(path: Block.Path): Kleisli[IO, Config, Resp[Unit]] = Kleisli { config ⇒
    IO {
      new File(path.file.value.getPath).delete()
      ()
    }.attempt
  }

  override protected[this] def seek(blockFile: Block.BlockFile, pos: Long): Kleisli[IO, Config, Resp[Unit]] = Kleisli {
    config ⇒
      IO {
        blockFile.seek(pos)
      }.attempt
  }

  override protected[this] def read(blockFile: Block.BlockFile,
                                    size: Block.Size): Kleisli[IO, Config, Resp[ByteBuffer]] =
    Kleisli { config ⇒
      IO {
        blockFile.read(size.sizeInByte)
      }.attempt
    }

  override protected[this] def write(blockFile: Block.BlockFile, data: ByteBuffer): Kleisli[IO, Config, Resp[Unit]] =
    Kleisli { config ⇒
      IO {
        blockFile.write(data)
      }.attempt
    }

  override protected[this] def lock(blockFile: Block.BlockFile): Kleisli[IO, Config, Resp[Boolean]] = Kleisli { config ⇒
    IO {
      val lock = blockFile.lock()
      if (lock == null) false
      else {
        lockMap.putIfAbsent(blockFile, lock)
        true
      }
    }.attempt
  }

  override protected[this] def unlock(blockFile: Block.BlockFile): Kleisli[IO, Config, Resp[Unit]] = Kleisli { config ⇒
    IO {
      val lock = lockMap.get(blockFile)
      if (lock != null)
        lock.release()
      else ()
    }.attempt
  }

  override protected[this] def existed(path: Block.Path): Kleisli[IO, Config, Resp[Boolean]] = Kleisli { config ⇒
    IO {
      new File(path.file.value.getPath).exists()
    }.attempt
  }
}

object BlockInterpreter {

  trait Config

  def config(): Config = new Config {}

  def apply(): BlockInterpreter = new BlockInterpreter {}
}
