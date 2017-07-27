/**
  * core_read.scala
  * ----------
  * read program definition
  * @author bigknife
  * @since 2017/7/27
  */
package ufs3
package core
package read

import java.io.IOException
import java.nio.ByteBuffer

import cats.Id
import cats.data.Kleisli
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import sop._
import ufs3.kernel.sandwich.SandwichOut
import core.data.Data._
import scala.language.higherKinds

trait ReadProgram {

  def readWithKey[F[_], Out](key: String, from: UFS3, to: Out)(implicit B: Block[F],
                                                               F: Filler[F],
                                                               FI: Fildex[F],
                                                               S: SandwichOut[F, Out],
                                                               L: Log[F]): Kleisli[Id, CoreConfig, SOP[F, Unit]] =
    Kleisli { coreConfig ⇒
      import ufs3.kernel.block.Block.Size._
      import L._
      val prog: Id[SOP[F, Unit]] = for {
        optIdx ← FI.fetchKey(key, from.fildexFile.get())
        _      ← debug(s"key($key) idx is $optIdx")
        _ ← if (optIdx.nonEmpty) {
          val startPoint = optIdx.get.startPoint
          val endPoint   = optIdx.get.endPoint

          def read(pos: Long, length: Long): SOP[F, ByteBuffer] =
            for {
              _  ← B.seek(from.blockFile.get(), pos)
              bb ← B.read(from.blockFile.get(), length.B)
            } yield bb

          def readBody(pos: Long, length: Long, remain: Long): SOP[F, Unit] =
            for {
              _ ← B.seek(from.blockFile.get(), pos)
              _ ← if (remain > 0) {
                val toRead = Math.min(length, remain)
                for {
                  bb ← B.read(from.blockFile.get(), toRead.B)
                  _  ← S.outputBody(bb, to)
                  _  ← readBody(pos + toRead, length, remain - toRead)
                } yield ()
              } else SOP.pure[F, Unit](())
            } yield ()
          // 1. read head
          // 2. read body recursively
          // 3. read tail
          for {
            headSize ← S.headSize()
            tailSize ← S.tailSize()
            _        ← debug(s"start: ${startPoint + headSize}, remain: ${endPoint - startPoint - headSize - tailSize}")
            headB    ← read(startPoint, headSize)
            _        ← S.head(headB, to)
            _ ← readBody(startPoint + headSize,
                         coreConfig.fillerReadBufferSize.sizeInByte,
                         endPoint - startPoint - headSize - tailSize)
            tailB ← read(endPoint - tailSize, tailSize)
            _     ← S.tail(tailB, to)
          } yield ()

        } else throw new IOException(s"no key=$key found")
      } yield ()
      prog
    }
}
object ReadProgram extends ReadProgram
