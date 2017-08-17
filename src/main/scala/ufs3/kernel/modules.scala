package ufs3.kernel

import freestyle._
import freestyle.effects.error._
import freestyle.effects.reader
import ufs3.kernel.algebras._
import ufs3.kernel.commons.Config

object modules {
  val rd = reader[Config]

  @module trait Store {
      val block: Block
      val filler: Filler
      val fildex: Fildex
    }

  @module trait App {
    val store: Store
    val sandwich: Sandwich

    val errorM: ErrorM
    val readerM: rd.ReaderM
    val log: Log

    val byteBufferStream: ByteBufferStream
  }
}