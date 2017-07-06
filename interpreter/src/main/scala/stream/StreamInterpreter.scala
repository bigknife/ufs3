package stream

import java.io.{FileInputStream, InputStream}
import java.nio.ByteBuffer

import cats.{Id, ~>}
import ufs3.kernel.store.WriteData
import ufs3.kernel.stream.{Data, Stream}
import ufs3.kernel.stream.Stream.Next

/**
  * Created by songwenchao on 2017/7/5.
  */
object StreamInterpreter {

  def streamPathInterpreter(path: String): Stream ~> Id = new (Stream ~> Id) {
    val inputStream        = new FileInputStream(path)
    val array              = new Array[Byte](8192)
    val buffer: ByteBuffer = ByteBuffer.allocate(8192)
    override def apply[A](fa: Stream[A]): Id[A] = fa match {
      case Next ⇒
        val length = inputStream.read(array)
        if (length != -1) {
          buffer.put(array, 0, length)
          val data = Data(buffer)
          Some(data)
        } else {
          inputStream.close()
          None
        }
    }
  }

  def streamInterpreter(input: InputStream): Stream ~> Id = new (Stream ~> Id) {
    val array              = new Array[Byte](8192)
    val buffer: ByteBuffer = ByteBuffer.allocate(8192)
    override def apply[A](fa: Stream[A]): Id[A] = fa match {
      case Next ⇒
        val length = input.read(array)
        if (length != -1) {
          buffer.put(array, 0, length)
          val data = Data(buffer)
          Some(data)
        } else {
          input.close()
          None
        }
    }
  }

}
