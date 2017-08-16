package ufs3.interp

import java.io.InputStream
import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicReference

import ufs3.kernel.algebras.ByteBufferStream
import ufs3.kernel.commons._

import scala.language.implicitConversions

object byteBufferStream {
  import commons._

  val bufferMap = new AtomicReference[Map[InputStream, Array[Byte]]](Map.empty)

  implicit val byteBufferStreamInterp: ByteBufferStream.Handler[Stack] = new ByteBufferStream.Handler[Stack] {
    protected[this] def read(in: InputStream): Stack[Option[ByteBuffer]] = Stack.now[Option[ByteBuffer]] { config ⇒
      val buffer = (bufferMap.get.get(in) orElse {
        val _buffer = new Array[Byte](config.fillerReadBufferSize.sizeInByte.toInt)
        bufferMap.set(bufferMap.get() + (in → _buffer))
        Some(_buffer)
      }).get

      in.read(buffer) match {
        case -1 ⇒ None
        case x  ⇒ Some(ByteBuffer.wrap(buffer, 0, x))
      }
    }
  }
}
