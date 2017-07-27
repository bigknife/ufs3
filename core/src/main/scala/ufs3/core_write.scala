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

trait WriteProgam {
  // write a in: IN to UFS3
  // 1. filler file allocate a start point for new sandwich (key → sandwich), got a start point
  // 1. write sandwich head
  // 2. travese sandwich body and write them
  // 3. write sandwich tail
  // Done: no index, no backup, no lock will be fixed at #17: add backup logic to core dsl
  def write[F[_], IN](key: String, in: IN, out: UFS3)(implicit B: Block[F],
                                                      F: Filler[F],
                                                      //BAK: Backup[F],
                                                      L: Log[F],
                                                      FI: Fildex[F],
                                                      S: SandwichIn[F, IN]): Kleisli[Id, CoreConfig, SOP[F, String]] =
  Kleisli { coreConfig ⇒
    import L._
    def prog(md: MessageDigest): Id[SOP[F, String]] =
      for {
        writing ← F.isWriting(out.fillerFile.get())
        optIdx ← if (writing) {
          for {
            _ ← warn(s"ufs3 is writing, please retry later! requesting key = $key")
          } yield throw new IllegalAccessException("ufs3 is writing, please retry later")
          //throw new IllegalAccessException("ufs3 is writing, please retry later")
        } else FI.fetchKey(key, out.fildexFile.get())
        _ ← info(s"writing for key: $key")
        startPos ← if (optIdx.isEmpty) F.startAppend(out.fillerFile.get())
        else {
          for {
            _ ← error(s"stop writing, $key has existed in ufs3")
          } yield throw new IllegalArgumentException(s"$key has existed in ufs3")

        }

        headB ← S.head(key, EmptyUUID, 0) //length can't be detected
        bodyLength ← {
          def writeBody(md5: MessageDigest, pos: Long): SOP[F, Long] =
            for {
              obb ← S.nextBody(in)
              nextLength ← if (obb.nonEmpty) {
                md5.update(obb.get)
                for {
                  _ ← B.seek(out.blockFile.get(), pos)
                  _ ← B.write(out.blockFile.get(), obb.get)
                  a ← writeBody(md5, pos + obb.get.limit())
                  _ ← debug(s"writing $key, from $pos, length: ${obb.get.limit()}")
                } yield a
              } else
                Par.pure[F, Long](0): SOP[F, Long] // implicitly transformed by liftPAR_to_SOP
            //_ ← if (obb.nonEmpty) BAK.send(obb.get) else Par.pure[F, Unit](()) // backup body
            } yield nextLength + obb.map(_.limit()).getOrElse(0)
          // 从 startpos + headB 的地方写入
          writeBody(md, startPos + headB.limit())
        }
        md5   ← SOP.pure[F, String](md.digest().map("%02x".format(_)).mkString(""))
        headB ← S.head(key, md5, bodyLength)
        _     ← B.seek(out.blockFile.get(), startPos)
        _     ← B.write(out.blockFile.get(), headB)
        _     ← debug(s"writed $bodyLength bytes with key = $key, md5 = $md5")
        tailB ← S.tail(md5.getBytes("iso8859_1"), bodyLength)
        _     ← B.seek(out.blockFile.get(), startPos + headB.limit() + bodyLength)
        _     ← B.write(out.blockFile.get(), tailB)
        newFildexFile ← FI.append(out.fildexFile.get(),
          Idx(key, md5, startPos, startPos + headB.limit() + bodyLength + tailB.limit()))
        newFillerFile ← F.endAppend(out.fillerFile.get(),
          startPos,
          startPos + headB.limit() + bodyLength + tailB.limit())
        _ ← info(
          s"writed filler and fildex with key: $key. startPos: $startPos, endPos: ${startPos + headB.limit() + bodyLength + tailB
            .limit()}")
      } yield {
        out.fillerFile.set(newFillerFile)
        out.fildexFile.set(newFildexFile)
        md5
      }
    prog(MessageDigest.getInstance("md5"))
  }

}

object WriteProgam extends WriteProgam