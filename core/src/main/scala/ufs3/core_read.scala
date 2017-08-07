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
import RespSOP._

trait ReadProgram {

  def readWithKey[F[_], Out](key: String, from: UFS3, to: Out)(implicit B: Block[F],
                                                               F: Filler[F],
                                                               FI: Fildex[F],
                                                               S: SandwichOut[F, Out],
                                                               L: Log[F]): Kleisli[Id, CoreConfig, RespSOP[F, Unit]] =
    Kleisli { coreConfig ⇒
      import ufs3.kernel.block.Block.Size._
      import L._

      def readBody(pos: Long, length: Long, remain: Long): RespSOP[F, Unit] =
        for {
          _ ← B.seek(from.blockFile.get(), pos).asM
          _ ← if (remain > 0) {
            val toRead = Math.min(length, remain)
            val s: RespSOP[F, Unit] = for {
              bb ← B.read(from.blockFile.get(), toRead.B).asM
              _  ← S.outputBody(bb, to).asM
              _  ← readBody(pos + toRead, length, remain - toRead).asM
              //_ ← info(s"reading and output to S $toRead Bytes, remain $remain Bytes").asM
            } yield ()
            s.asM
          } else RespSOP.pure[F, Unit](()).asM
        } yield ()

      def read(pos: Long, length: Long): RespSOP[F, ByteBuffer] =
        for {
          _  ← B.seek(from.blockFile.get(), pos).asM
          bb ← B.read(from.blockFile.get(), length.B).asM
        } yield bb

      val prog: Id[RespSOP[F, Unit]] = for {
        optIdx ← FI.fetchKey(key, from.fildexFile.get()).asM
        _      ← debug(s"key($key) idx is $optIdx").asM
        _ ← if (optIdx.nonEmpty) {
          val startPoint = optIdx.get.startPoint
          val endPoint   = optIdx.get.endPoint
          // 1. read head
          // 2. read body recursively
          // 3. read tail
          val s: RespSOP[F, Unit] = for {
            headSize ← S.headSize().asM
            tailSize ← S.tailSize().asM
            _        ← debug(s"start: ${startPoint + headSize}, remain: ${endPoint - startPoint - headSize - tailSize}").asM
            headB    ← read(startPoint, headSize).asM
            _        ← S.head(headB, to).asM
            _ ← readBody(startPoint + headSize,
                         coreConfig.fillerReadBufferSize.sizeInByte,
                         endPoint - startPoint - headSize - tailSize).asM
            tailB ← read(endPoint - tailSize, tailSize).asM
            _     ← S.tail(tailB, to).asM
          } yield ()
          s.asM
        } else RespSOP.error[F, Unit](new IOException(s"no key=$key found")).asM
      } yield ()
      prog
    }
}
object ReadProgram extends ReadProgram
