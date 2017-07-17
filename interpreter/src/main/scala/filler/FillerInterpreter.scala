package filler

import cats.Id
import ufs3.kernel.block.Block
import ufs3.kernel.filler.Filler

/**
  * Created by songwenchao on 2017/7/14.
  */
object FillerInterpreter extends Filler.Handler[Id] {
  override def init(blockFile: Block.BlockFile): Id[Filler.FillerFile] = ???

  override def check(blockFile: Block.BlockFile): Id[Filler.FillerFile] = ???

  override def close(ff: Filler.FillerFile): Id[Unit] = ???
}
