/**
  * core_write.scala
  * ----------
  * write program
  * @author bigknife
  * @since 2017/7/27
  */
package ufs3
package core
package write

import java.security.MessageDigest

import cats.Id
import cats.data.Kleisli
import sop._
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.fildex.Fildex.Idx
import ufs3.kernel.filler.Filler
import ufs3.kernel.log.Log
import ufs3.kernel.sandwich.SandwichIn
import core.data.Data._
import scala.language.higherKinds
import RespSOP._

trait WriteProgam {
  def isWritable[F[_]](out: UFS3)(implicit F: Filler[F]): Kleisli[Id, CoreConfig, RespSOP[F, Unit]] =
    Kleisli { config ⇒
      val prog: Id[RespSOP[F, Unit]] = for {
        writing ← F.isWriting(out.fillerFile.get()).asM
        _ ← if (writing) RespSOP.error[F, Unit](new IllegalStateException("ufs3 is writing, please retry later")).asM
        else RespSOP.pure[F, Unit](()).asM
      } yield ()
      prog
    }

  def existedKey[F[_]](key: String, out: UFS3)(implicit FI: Fildex[F]): Kleisli[Id, CoreConfig, RespSOP[F, Boolean]] =
    Kleisli { config ⇒
      val prog: Id[RespSOP[F, Boolean]] = for {
        optIdx ← FI.fetchKey(key, out.fildexFile.get()).asM
      } yield optIdx.isDefined
      prog
    }

  def forceToWrite[F[_]](out: UFS3)(implicit F: Filler[F]): Kleisli[Id, CoreConfig, RespSOP[F, Unit]] = Kleisli { config ⇒
    val prog: Id[RespSOP[F, Unit]] = F.forceToWrite(out.fillerFile.get())
    prog
  }

  // write a in: IN to UFS3
  // 1. filler file allocate a start point for new sandwich (key → sandwich), got a start point
  // 1. write sandwich head
  // 2. travese sandwich body and write them
  // 3. write sandwich tail
  // Done: no index, no backup, no lock will be fixed at #17: add backup logic to core dsl
  def write[F[_], IN](key: String, in: IN, out: UFS3)(
      implicit B: Block[F],
      F: Filler[F],
      //BAK: Backup[F],
      L: Log[F],
      FI: Fildex[F],
      S: SandwichIn[F, IN]): Kleisli[Id, CoreConfig, RespSOP[F, String]] =
    Kleisli { coreConfig ⇒
      import L._

      def writeBody(md5: MessageDigest, pos: Long): RespSOP[F, Long] =
        for {
          obb ← S.nextBody(in).asM
          nextLength ← if (obb.nonEmpty) {
            (for {
              _ ← RespSOP.pure[F, Unit](md5.update(obb.get)).asM
              _ ← B.seek(out.blockFile.get(), pos).asM
              _ ← B.write(out.blockFile.get(), obb.get).asM
              a ← writeBody(md5, pos + obb.get.limit()).asM
              //_ ← debug(s"writing $key, from $pos, length: ${obb.get.limit()}")
            } yield a).asM
          } else RespSOP.pure[F, Long](0).asM
        } yield nextLength + obb.map(_.limit()).getOrElse(0)

      def md5Value(md: MessageDigest): String = md.digest().map("%02x".format(_)).mkString("")

      def prog(md: MessageDigest): Id[RespSOP[F, String]] =
        for {
          writing ← F.isWriting(out.fillerFile.get()).asM
          optIdx ← if (writing) {
            RespSOP.error[F, Option[Idx]](new IllegalAccessException("ufs3 is writing, please retry later")).asM
          } else FI.fetchKey(key, out.fildexFile.get()).asM
          _ ← info(s"writing for key: $key").asM
          startPos ← if (optIdx.isEmpty) F.startAppend(out.fillerFile.get()).asM
          else RespSOP.error[F, Long](new IllegalArgumentException(s"$key has existed in ufs3")).asM
          headB      ← S.head(key, EmptyUUID, 0).asM //length can't be detected, 预先设定为0
          bodyLength ← writeBody(md, startPos + headB.limit()).asM
          md5        ← RespSOP.pure[F, String](md5Value(md)).asM
          headB      ← S.head(key, md5, bodyLength).asM
          _          ← B.seek(out.blockFile.get(), startPos).asM
          _          ← B.write(out.blockFile.get(), headB).asM
          _          ← debug(s"writed $bodyLength bytes with key = $key, md5 = $md5").asM
          tailB      ← S.tail(md5.getBytes("iso8859_1"), bodyLength).asM
          _          ← B.seek(out.blockFile.get(), startPos + headB.limit() + bodyLength).asM
          _          ← B.write(out.blockFile.get(), tailB).asM
          newFildexFile ← FI
            .append(out.fildexFile.get(),
                    Idx(key, md5, startPos, startPos + headB.limit() + bodyLength + tailB.limit()))
            .asM
          newFillerFile ← F
            .endAppend(out.fillerFile.get(), startPos, startPos + headB.limit() + bodyLength + tailB.limit())
            .asM
          _ ← info(
            s"writed filler and fildex with key: $key. startPos: $startPos, endPos: ${startPos + headB.limit() + bodyLength + tailB
              .limit()}").asM
        } yield {
          out.fillerFile.set(newFillerFile)
          out.fildexFile.set(newFildexFile)
          md5
        }
      prog(MessageDigest.getInstance("md5"))
    }

}

object WriteProgam extends WriteProgam
