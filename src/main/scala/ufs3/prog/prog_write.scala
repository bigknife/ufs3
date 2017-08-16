package ufs3.prog

import java.io.InputStream
import java.nio.ByteBuffer
import java.security.MessageDigest

import scala.language.higherKinds
import freestyle._
import ufs3.kernel.commons._
import ufs3.kernel.modules._
import _root_.fs2.io._
import _root_.fs2.util._
import cats.effect.IO
import freestyle.fs2.Eff
import _root_.fs2.Stream

object write {
  /*
  implicit val x = new Suspendable[IO] {
    def suspend[A](fa: => IO[A]) = IO.suspend(fa)

    def flatMap[A, B](a: IO[A])(f: (A) => IO[B]) = a.flatMap(f)

    def pure[A](a: A) = IO.pure(a)
  }
   */
  private[this] def md5hex(md: MessageDigest): String = md.digest().map("%02x".format(_)).mkString("")

  def apply[F[_]](key: String, in: InputStream, out: UFS3)(implicit app: App[F]): FreeS[F, String] = {
    def writeBody(md5: MessageDigest, pos: Long): FreeS[F, Long] = {
      for {
        obb ← app.byteBufferStream.read(in)
        nextLength ← if (obb.nonEmpty) {
          for {
            _ ← FreeS.pure[F, Unit](md5.update(obb.get))
            _ ← app.store.block.seek(out.blockFile.get(), pos)
            _ ← app.store.block.write(out.blockFile.get(), obb.get)
            a ← writeBody(md5, pos + obb.get.limit)
          } yield a
        } else FreeS.pure[F, Long](0)
      } yield nextLength + obb.map(_.limit()).getOrElse(0)
    }

    for {
      writing ← app.store.filler.isWriting(out.fillerFile.get())
      _ ← app.errorM.either(
        Either.cond(!writing, (), new IllegalAccessException("ufs3 is writing, please retry later")))

      optIdx ← app.store.fildex.fetchKey(key, out.fildexFile.get())
      _      ← app.errorM.either(Either.cond(optIdx.isEmpty, (), new IllegalArgumentException(s"$key has existed in ufs3")))

      startPos ← app.store.filler.startAppend(out.fillerFile.get())
      headB0   ← app.sandwich.head(key, EmptyUUID, 0) //length can't be detected, 预先设定为0

      md = MessageDigest.getInstance("md5")
      bodyLength ← writeBody(md, startPos + headB0.limit())

      md5 = md5hex(md)
      headB1 ← app.sandwich.head(key, md5, bodyLength)
      _      ← app.store.block.seek(out.blockFile.get(), startPos)
      _      ← app.store.block.write(out.blockFile.get(), headB1)

      md5Bytes = md5.getBytes("ISO8859_1")
      tailB ← app.sandwich.tail(md5Bytes, bodyLength)
      _     ← app.store.block.seek(out.blockFile.get(), startPos + headB1.limit() + bodyLength)
      _     ← app.store.block.write(out.blockFile.get(), tailB)

      endPos = startPos + headB1.limit() + bodyLength + tailB.limit()
      newFildexFile ← app.store.fildex.append(out.fildexFile.get(), Idx(key, md5, startPos, endPos))
      newFillerFile ← app.store.filler.endAppend(out.fillerFile.get(), startPos, endPos)

      _ = out.fillerFile.set(newFillerFile)
      _ = out.fildexFile.set(newFildexFile)
    } yield md5
  }
}
