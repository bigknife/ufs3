/**
  * sandwich_out.scala
  * ----------
  * interpreter of sandwich out
  * @author bigknife
  * @since 2017/7/17
  */
package ufs3
package interpreter
package sandwich

import java.io.{InputStream, OutputStream}
import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicReference

import cats.data.Kleisli
import cats.effect.IO
import ufs3.interpreter.layout.{SandwichHeadLayout, SandwichTailLayout}
import ufs3.interpreter.util._
import ufs3.kernel.sandwich._

trait SandwichOutInterpreter extends SandwichOut.Handler[Kleisli[IO, SandwichOutInterpreter.Config, ?], OutputStream] {
  case class Buffer(list: List[Byte])

  private[this] val cache = AtomicMap[OutputStream, Buffer]

  def headSize(): Kleisli[IO, SandwichOutInterpreter.Config, Long] = Kleisli { config ⇒
    IO { SandwichHeadLayout.HEAD_LENGTH.toLong }
  }

  def tailSize(): Kleisli[IO, SandwichOutInterpreter.Config, Long] = Kleisli { config ⇒
    IO { SandwichTailLayout.TAIL_LENGTH.toLong }
  }

  def head(bb: ByteBuffer, out: OutputStream): Kleisli[IO, SandwichOutInterpreter.Config, Unit] = Kleisli { config ⇒
    IO {
      if (bb.limit() == SandwichHeadLayout.HEAD_LENGTH) {
        // make cache
        cache += (out → Buffer(List.empty[Byte]))
        //out.write(bb.array())
        // head bytes don't be output
        ()
      } else throw new java.io.IOException(s"the Sandwich head size SHOULD BE ${SandwichHeadLayout.HEAD_LENGTH}")
    }
  }

  def outputBody(body: ByteBuffer, out: OutputStream): Kleisli[IO, SandwichOutInterpreter.Config, Unit] = Kleisli {
    config ⇒
      IO {
        val cached = cache(out)
        if (cached.isDefined) {
          // if buffered size + body size is below the `outputBufferSize` in the config, then  buffer it
          // or write buffered bytes and body, clear the buffer.
          // over the buffer size
          // write directly
          if (body.position() != 0) body.flip()
          val bytes = new Array[Byte](body.limit())
          body.get(bytes)

          if (cached.get.list.length + bytes.length > config.outputBufferSize) {
            if (cached.get.list.nonEmpty) {
              out.write(cached.get.list.toArray)
            }

            out.write(bytes)
            // clear the buffer
            cache += (out → Buffer(List.empty[Byte]))
          } else {

            def addedBuffer(): Option[Buffer] =
              cache(out)
                .map(bb ⇒ ByteBuffer.wrap(bb.list.toArray[Byte] ++ bytes))
                .map(x ⇒ Buffer(x.array().toList))
            cache += (out → addedBuffer().get)
          }
        }

      }
  }

  def tail(bb: ByteBuffer, out: OutputStream): Kleisli[IO, SandwichOutInterpreter.Config, Unit] = Kleisli { config ⇒
    IO {
      try {
        val bytes = new Array[Byte](bb.limit())
        if (bb.position() != 0) bb.flip()
        bb.get(bytes)
        if (bytes.length == SandwichTailLayout.TAIL_LENGTH) {
          // if has cache, write
          val cached = cache(out)
          if (cached.isDefined) out.write(cached.get.list.toArray)
          //out.write(bytes)
          out.flush()
        } else throw new java.io.IOException(s"the Sandwich tail size SHOULD BE ${SandwichTailLayout.TAIL_HASH_SIZE}")
      } finally {
        // remove cache
        cache -= out
      }
    }
  }
}

object SandwichOutInterpreter {
  trait Config {
    def outputBufferSize: Long
  }
  private[this] case object ConfigInstance extends Config {
    def outputBufferSize: Long = 8192
  }
  def config(): Config = ConfigInstance

  def apply(): SandwichOutInterpreter = new SandwichOutInterpreter {}
}

trait SandwichInInterpreter extends SandwichIn.Handler[Kleisli[IO, SandwichInInterpreter.Config, ?], InputStream] {
  def head(key: String, uuid: String, bodyLength: Long): Kleisli[IO, SandwichInInterpreter.Config, ByteBuffer] =
    Kleisli { config ⇒
      IO {
        SandwichHeadLayout(key, uuid, bodyLength).head.byteBuffer
      }
    }

  //WARN 避免每次进行分配
  //     线程不安全，在当前场景，buffer共享变量是单线程使用的， 可以共享
  val buffer = new AtomicReference[Option[Array[Byte]]](None)
  def nextBody(in: InputStream): Kleisli[IO, SandwichInInterpreter.Config, Option[ByteBuffer]] = Kleisli { config ⇒
    IO {
      val buff =
        if (buffer.get.isDefined) buffer.get.get
        else {
          val b = new Array[Byte](config.inputBufferSize)
          buffer.set(Some(b))
          b
        }
      val readed = in.read(buff)
      if (readed != -1) {
        Some(ByteBuffer.wrap(buff, 0, readed))
      } else {
        None
      }
    }

  }

  def tail(hash: Array[Byte], bodyLength: Long): Kleisli[IO, SandwichInInterpreter.Config, ByteBuffer] = Kleisli {
    config ⇒
      IO {
        SandwichTailLayout(hash, bodyLength).tail.byteBuffer
      }
  }
}

object SandwichInInterpreter {
  trait Config {
    def inputBufferSize: Int
  }

  def apply(): SandwichInInterpreter = new SandwichInInterpreter {}
}
