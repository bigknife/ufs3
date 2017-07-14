package ufs3
package kernel
package test
package iotest

import java.io.{FileInputStream, FileOutputStream, InputStream}
import java.nio.ByteBuffer

import cats._
import cats.free._
import cats.free.Free._

sealed trait StreamOp[A, I]
case class Next[I](in: I) extends StreamOp[Option[ByteBuffer], I]

object TestApp extends App {
  def next[I](in: I): Free[StreamOp[?, I], Option[ByteBuffer]] = liftF[StreamOp[?, I], Option[ByteBuffer]](Next[I](in))

  def program[I](in: I): Free[StreamOp[?, I], Unit] =
    for {
      obb ← next[I](in)
      _   ← if (obb.nonEmpty) program(in) else Free.pure[StreamOp[?, I], Unit](())
    } yield ()

  def interpreter(callback: ByteBuffer ⇒ Unit) = new (StreamOp[?, InputStream] ~> Id) {
    override def apply[A](fa: StreamOp[A, InputStream]): Id[A] = fa match {
      case Next(ins) ⇒
        val arr    = new Array[Byte](8192)
        val readed = ins.read(arr)
        if (readed == -1) None
        else {
          val bb = ByteBuffer.wrap(arr, 0, readed)
          callback(bb)
          Some(bb)
        }
    }
  }


  val inputStream  = new FileInputStream("/tmp/a.txt")
  val outputStream = new FileOutputStream("/tmp/b.txt")

  program[InputStream](inputStream).foldMap(interpreter { bb ⇒
    outputStream.write(bb.array())
  })
  inputStream.close()
  outputStream.close()
}
