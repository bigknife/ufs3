package ufs3.prog

import java.io.OutputStream
import java.nio.ByteBuffer

import ufs3.kernel.commons.UFS3
import ufs3.kernel.modules.App
import freestyle._
import ufs3.kernel.exceptions._

import scala.language.higherKinds

object read {
  def apply[F[_]](key: String, from: UFS3, out: OutputStream)(implicit app: App[F]): FreeS[F, Unit] = {
    import ufs3.kernel.commons.Size._
    val fildexFile = from.fildexFile.get()
    val blockFile  = from.blockFile.get()

    // read `length` bytes at `pos`
    def read(pos: Long, length: Long): FreeS[F, ByteBuffer] =
      for {
        _  ← app.store.block.seek(blockFile, pos)
        bb ← app.store.block.read(blockFile, length.B)
      } yield bb

    // read body recursively
    def readBody(pos: Long, length: Long, remain: Long): FreeS[F, Unit] =
      for {
        _ ← app.store.block.seek(blockFile, pos)
        _ ← if (remain > 0) {
          val toRead = Math.min(length, remain)
          val s: FreeS[F, Unit] = for {
            bb ← app.store.block.read(blockFile, toRead.B)
            _  ← app.byteBufferStream.writeBody(bb, out)
            _  ← readBody(pos + toRead, length, remain - toRead)
          } yield ()
          s
        } else FreeS.pure[F, Unit](())
      } yield ()

    for {
      bufferSize ← app.readerM.reader(_.fillerReadBufferSize)
      optIdx     ← app.store.fildex.fetchKey(key, fildexFile)
      _          ← app.errorM.either(Either.cond(optIdx.isDefined, (), KeyNotFoundException(key)))
      (startPos, endPos) = optIdx.map(x ⇒ (x.startPoint, x.endPoint)).get
      headSize ← app.sandwich.headSize()
      tailSize ← app.sandwich.tailSize()
      headB    ← read(startPos, headSize)
      _        ← app.byteBufferStream.writeHead(headB, out)

      // read body
      totalBodySize = endPos - startPos - headSize - tailSize
      _ ← readBody(startPos + headSize, bufferSize.sizeInByte, totalBodySize)

      tailB ← read(endPos - tailSize, tailSize)
      _     ← app.byteBufferStream.writeTail(tailB, out)
    } yield ()
  }
}
