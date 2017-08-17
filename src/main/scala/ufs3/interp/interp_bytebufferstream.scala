package ufs3.interp

import java.io.{InputStream, OutputStream}
import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicReference

import ufs3.kernel.algebras.ByteBufferStream
import ufs3.kernel.commons._

import scala.language.implicitConversions

object byteBufferStream {
  import commons._

  val bufferMap = new AtomicReference[Map[InputStream, Array[Byte]]](Map.empty)

  implicit val byteBufferStreamInterp: ByteBufferStream.Handler[Stack] = new ByteBufferStream.Handler[Stack] {
    protected[this] def read(in: InputStream): Stack[Option[ByteBuffer]] = Stack.delay[Option[ByteBuffer]] { config ⇒
      val buffer = (bufferMap.get.get(in) orElse {
        val _buffer = new Array[Byte](config.fillerReadBufferSize.sizeInByte.toInt)
        bufferMap.set(bufferMap.get() + (in → _buffer))
        Some(_buffer)
      }).get

      in.read(buffer) match {
        case -1 ⇒
          //clean the cache
          bufferMap.set(bufferMap.get() - in)
          None
        case x  ⇒ Some(ByteBuffer.wrap(buffer, 0, x))
      }
    }

    import ufs3.kernel.utils._
    case class Buffer(list: List[Byte])
    private[this] val cache = AtomicMap[OutputStream, Buffer]

    protected[this] def writeHead(data: ByteBuffer, out: OutputStream): Stack[Unit] = Stack.configless[Unit] {
      if (data.limit() == SandwichHeadLayout.HEAD_LENGTH) {
        // make cache
        cache += (out → Buffer(List.empty[Byte]))
      } else throw new java.io.IOException(s"the Sandwich head size SHOULD BE ${SandwichHeadLayout.HEAD_LENGTH}")
    }

    protected[this] def writeBody(data: ByteBuffer, out: OutputStream): Stack[Unit] =Stack.delay[Unit]  { config ⇒
      val cached = cache(out)
      if (cached.isDefined) {
        // if buffered size + body size is below the `outputBufferSize` in the config, then  buffer it
        // or write buffered bytes and body, clear the buffer.
        // over the buffer size
        // write directly
        if (data.position() != 0) data.flip()
        val bytes = new Array[Byte](data.limit())
        data.get(bytes)

        if (cached.get.list.length + bytes.length > config.fillerReadBufferSize.sizeInByte.toInt) {
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
      } else throw new IllegalStateException(s"cache not found for the out:$out")
    }

    protected[this] def writeTail(data: ByteBuffer, out: OutputStream): Stack[Unit] = Stack.configless[Unit] {
      try {
        val bytes = new Array[Byte](data.limit())
        if (data.position() != 0) data.flip()
        data.get(bytes)
        if (bytes.length == SandwichTailLayout.TAIL_LENGTH) {
          // if has cache, write
          val cached = cache(out)
          if (cached.isDefined) out.write(cached.get.list.toArray)
          //out.write(bytes)
          Right(out.flush())
        } else throw new java.io.IOException(s"the Sandwich tail size SHOULD BE ${SandwichTailLayout.TAIL_HASH_SIZE}")
      } finally {
        // remove cache
        cache -= out
      }
    }
  }
}
