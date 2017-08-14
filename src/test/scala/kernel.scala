package ufs3.kernel
import java.nio.ByteBuffer

import org.scalatest.FlatSpec
import algebras.{Block, _}
import cats.Id
import commons._
import freestyle._


import scala.language.higherKinds

class BlockSpec extends FlatSpec {
  "A Non-Existed path" should "return false" in {
    implicit val block = Block[Block.Op]

    implicit val handler = new Block.Handler[Id] {
      protected[this] def existed(path: Path) = true

      protected[this] def open(path: Path, mode: FileMode) = ???

      protected[this] def close(bf: BlockFile) = ???

      protected[this] def create(path: Path, size: Size) = ???

      protected[this] def delete(path: Path) = ???

      protected[this] def seek(bf: BlockFile, pos: Long) = ???

      protected[this] def read(bf: BlockFile, size: Size) = ???

      protected[this] def write(bf: BlockFile, data: ByteBuffer) = ???

      protected[this] def lock(bf: BlockFile) = ???

      protected[this] def unlock(bf: BlockFile) = ???
    }

    def existed[F[_]](implicit B: Block[F]): FreeS[F, Boolean] = for {
      b ← B.existed(Path("/NonExisted"))
    } yield b

    println(existed.interpret[Id])
  }
}