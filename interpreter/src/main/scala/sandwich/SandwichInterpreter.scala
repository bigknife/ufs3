package sandwich

import java.io.InputStream
import java.nio.ByteBuffer

import cats.Id
import ufs3.kernel.sandwich.Sandwich

/**
  * Created by songwenchao on 2017/7/14.
  */
object SandwichInterpreter extends Sandwich.Handler[Id, InputStream] {

  override def head(): Id[ByteBuffer] = {
    val headMagic = "head".getBytes("utf-8")
    ByteBuffer.wrap(headMagic, 0, headMagic.length)
  }

  override def nextBody(in: InputStream): Id[Option[ByteBuffer]] = {
    val array = new Array[Byte](8192)
    val read  = in.read(array)
    if (read == -1) None
    else Some(ByteBuffer.wrap(array, 0, read))
  }

  override def tail(): Id[ByteBuffer] = {
    val tailMagic = "tail".getBytes("utf-8")
    ByteBuffer.wrap(tailMagic, 0, tailMagic.length)
  }
}
