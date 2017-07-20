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
import java.security.MessageDigest

import cats.data.Kleisli
import cats.effect.IO
import kernel.sandwich._
import util._

object Sandwich {
  // the size of Sandwich-Head is 52 Bytes
  // [0 - 4): the magic, 'UFS3'
  // [4 - 12): the create time
  // [12 - 44): the key of the file
  // [44 - 52): the body length
  val HEAD_MAGIC: Array[Byte] = "UFS3".getBytes
  val HEAD_LENGTH: Int        = 52

  // the size of Sandwich-Tail is 16 Bytes, the hash of the body
  val HASH_SIZE: Int = 32
}

trait SandwichOutInterpreter extends SandwichOut.Handler[Kleisli[IO, SandwichOutInterpreter.Config, ?], OutputStream] {
  case class Buffer(list: List[Byte])

  private[this] val cache = AtomicMap[OutputStream, Buffer]

  //todo: sandwich layout should be refactored

  def headSize(): Kleisli[IO, SandwichOutInterpreter.Config, Long] = Kleisli { config ⇒
    IO { Sandwich.HEAD_LENGTH.toLong }
  }

  def tailSize(): Kleisli[IO, SandwichOutInterpreter.Config, Long] = Kleisli { config ⇒
    IO { Sandwich.HASH_SIZE.toLong + 8L }
  }

  def head(bb: ByteBuffer, out: OutputStream): Kleisli[IO, SandwichOutInterpreter.Config, Unit] = Kleisli { config ⇒
    IO {
      if (bb.limit() == Sandwich.HEAD_LENGTH) {
        // make cache
        cache += (out → Buffer(List.empty[Byte]))
        //out.write(bb.array())
        // head bytes don't be output
        ()
      } else throw new java.io.IOException(s"the Sandwich head size SHOULD BE ${Sandwich.HEAD_LENGTH}")
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
        if(bb.position() != 0) bb.flip()
        bb.get(bytes)
        //todo : refactor sandwich layout
        if (bytes.length == Sandwich.HASH_SIZE + 8) {
          // if has cache, write
          val cached = cache(out)
          if (cached.isDefined) out.write(cached.get.list.toArray)
          //out.write(bytes)
          out.flush()
        } else throw new java.io.IOException(s"the Sandwich tail size SHOULD BE ${Sandwich.HASH_SIZE}")
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


object T {
  var s: Int = 0
}

trait SandwichInInterpreter extends SandwichIn.Handler[Kleisli[IO, SandwichInInterpreter.Config, ?], InputStream] {
  def head(key: String, bodyLength: Long): Kleisli[IO, SandwichInInterpreter.Config, ByteBuffer] = Kleisli { config ⇒
    IO {
      val bb = ByteBuffer.allocate(Sandwich.HEAD_LENGTH)
      bb.put(Sandwich.HEAD_MAGIC)            // 4bytes magic
      bb.putLong(System.currentTimeMillis()) // 8bytes create time
      //val keyHash = MessageDigest.getInstance("MD5").digest(key.getBytes("UTF-8"))
      bb.put(key.getBytes("iso8859_1"))        // 32bytes key md5 hash
      bb.putLong(bodyLength) // 8bytes body length
      bb.flip()
      bb
    }
  }

  def nextBody(in: InputStream): Kleisli[IO, SandwichInInterpreter.Config, Option[ByteBuffer]] = Kleisli { config ⇒
    IO {
      val buff   = new Array[Byte](config.inputBufferSize)
      val readed = in.read(buff)
      if (readed != -1) {
        T.s = T.s + readed
        Some(ByteBuffer.wrap(buff, 0, readed))
      }
      else {
        println(s"读入了： ${T.s}")
        None
      }
    }

  }

  def tail(hash: Array[Byte], bodyLength: Long): Kleisli[IO, SandwichInInterpreter.Config, ByteBuffer] = Kleisli {
    config ⇒
      IO {
        import ufs3.interpreter.layout.Layout._
        require(hash.length == Sandwich.HASH_SIZE, s"the Sandwich tail size SHOULD BE ${Sandwich.HASH_SIZE}")
        ByteBuffer.wrap(hash ++ bodyLength.`8Bytes`.bytes)
      }
  }
}

object SandwichInInterpreter {
  trait Config {
    def inputBufferSize: Int
  }

  def apply(): SandwichInInterpreter = new SandwichInInterpreter {}
}
