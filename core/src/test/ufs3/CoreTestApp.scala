package ufs3

import cats.data.Coproduct
import ufs3.kernel.block.Block
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.filler.Filler

/**
  * Created by songwenchao on 2017/7/20.
  */
object CoreTestApp extends App {

  import core._

  type APP[A]       = Coproduct[Block.Op, Filler.Op, A]
  type SecondApp[A] = Coproduct[Fildex.Op, APP, A]

  startup[SecondApp]

}
