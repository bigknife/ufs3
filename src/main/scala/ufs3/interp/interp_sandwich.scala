package ufs3.interp

import java.nio.ByteBuffer
import ufs3.kernel.algebras.Sandwich
import ufs3.kernel.commons._

import scala.language.implicitConversions

object sandwich {
  import commons._

  implicit val sandwichInterp = new Sandwich.Handler[Stack] {
    protected[this] def head(key: String, uuid: String, bodyLength: Long): Stack[ByteBuffer] = Stack.configless[ByteBuffer]{
      SandwichHeadLayout(key, uuid, bodyLength).head.byteBuffer
    }

    protected[this] def tail(hash: Array[Byte], bodyLength: Long): Stack[ByteBuffer] = Stack.configless[ByteBuffer]{
      SandwichTailLayout(hash, bodyLength).tail.byteBuffer
    }

    protected[this] def headSize: Stack[Long] = Stack.configless[Long](SandwichHeadLayout.HEAD_LENGTH.toLong)

    protected[this] def tailSize: Stack[Long] = Stack.configless[Long](SandwichTailLayout.TAIL_LENGTH.toLong)
  }
}
