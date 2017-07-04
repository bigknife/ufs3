/**
  * api.scala
  * ---------
  * small programs composed with kernel's adt
  * @author : bigknife@outlook.com
  * @since 2017/07/03
  */
package ufs3
package core

import cats.data.Coproduct
import cats.free.Free
import ufs3.kernel.audit._
import ufs3.kernel.backup._
import ufs3.kernel.log._
import ufs3.kernel.store._
import ufs3.kernel.stream._

import scala.language.higherKinds

object api {
  type LS[A]    = Coproduct[Log, Store, A]
  type ALS[A]   = Coproduct[Audit, LS, A]
  type BALS[A]  = Coproduct[Backup, ALS, A]
  type SBALS[A] = Coproduct[Stream, BALS, A]
  type Api[A]   = SBALS[A]

  import Store.Response

  private[this] def freeError[F[_], A](t: Throwable): Free[F, Response[A]] =
    Free.pure[F, Response[A]](Left(t))
  private[this] def freeResponse[F[_], A](a: A): Free[F, Response[A]] =
    Free.pure[F, Response[A]](Right(a))

  /**
    * create filler file if not existed
    */
  def createIfNotExisted[F[_]](path: Path, size: Size)(implicit L: Log.Ops[F],
                                                       S: Store.Ops[F]): Free[F, Response[Unit]] = {
    import L._
    import S._
    import cats.syntax.either._
    for {
      _  ← debug(s"checking if existed: $path")
      rb ← existed(path)
      c ← rb match {
        case Left(t) ⇒
          for {
            _ ← error("checking if existfed failed", t)
            a ← freeError[F, Unit](t)
          } yield a
        case Right(false) ⇒
          for {
            _ ← info(s"not found: $path")
            b ← create(path, size)
            _ ← info(s"create file: $path with size: $size")
          } yield b.map(_ ⇒ ())
        case Right(true) ⇒ freeResponse[F, Unit](())

      }
    } yield c
  }

  /**
    * open filler and lock it
    */
  def openForWrite[F[_]](path: Path)(implicit L: Log.Ops[F], S: Store.Ops[F]): Free[F, Response[WritableFiller]] = {
    import L._
    import S._
    for {
      rf ← open(path, FileMode.ReadWrite)
      row ← rf match {
        case Left(t) ⇒
          error(s"open readwrite file $path failed", t).flatMap(_ ⇒ freeError[F, Option[WritableFiller]](t))
        case Right(filler) ⇒ debug(s"opened readwrite file $path").flatMap(_ ⇒ writable(filler))
      }
      rw ← row match {
        case Left(t) ⇒ freeError[F, WritableFiller](t)
        case Right(Some(w)) ⇒
          for {
            rl ← lock(w)
            a ← if (rl.isRight)
              debug(s"try to lock $path").flatMap(_ ⇒ freeResponse[F, WritableFiller](w))
            else freeError[F, WritableFiller](new IllegalAccessError(s"can't lock $path"))
          } yield a
        case Right(None) ⇒ freeError[F, WritableFiller](new IllegalStateException(s"$path is not writable"))
      }
    } yield rw
  }

  /**
    * append data stream
    */
  def appendData[F[_]](filler: Filler)(implicit L: Log.Ops[F],
                                       S: Store.Ops[F],
                                       STREAM: Stream.Ops[F],
                                       B: Backup.Ops[F]): Free[F, Response[Unit]] = {
    import S._
    import STREAM._
    import Stream.Ops._
    for {
      a ← {
        def streaming(): Free[F, Response[Unit]] = {
          for {
            dataOpt ← next
            _ ← dataOpt match {
              case Some(data) ⇒
                for {
                  // write and send to backup
                  _ ← write(filler, data.to[ufs3.kernel.store.Data]).flatMap(_ ⇒
                    B.send(data.to[ufs3.kernel.backup.Data]))
                  a ← streaming()
                } yield a
              case None ⇒ freeResponse[F, Unit](())
            }
          } yield Right(())
        }
        streaming()
      }
    } yield a
  }

  /**
    * unlock and close
    */
  def unlockAndClose[F[_]](filler: Filler)(implicit L: Log.Ops[F], S: Store.Ops[F]): Free[F, Response[Unit]] = {
    import S._
    for {
      _ ← unlock(filler)
      _ ← close(filler)
    } yield Right(())
  }

  /**
    * open for read
    */
  def openForRead[F[_]](path: Path)(implicit L: Log.Ops[F], S: Store.Ops[F]): Free[F, Response[ReadonlyFiller]] = {
    import L._, S._
    for {
      rf ← open(path, FileMode.ReadOnly)
      row ← rf match {
        case Left(t) ⇒
          error(s"open readonly file $path failed", t).flatMap(_ ⇒ freeError[F, Option[ReadonlyFiller]](t))
        case Right(filler) ⇒ debug(s"opend readonly file $path").flatMap(_ ⇒ readable(filler))
      }
      rw ← row match {
        case Left(t)             ⇒ freeError[F, ReadonlyFiller](t)
        case Right(Some(filler)) ⇒ freeResponse[F, ReadonlyFiller](filler)
        case Right(None)         ⇒ freeError[F, ReadonlyFiller](new IllegalStateException(s"$path is not readable"))
      }
    } yield rw
  }

}
