/**
  * core_data.scala
  * ----------
  * data structure
  * @author bigknife
  * @since 2017/7/27
  */

package ufs3
package core
package data

import java.util.concurrent.atomic.AtomicReference

import ufs3.kernel.block.Block
import ufs3.kernel.block.Block.BlockFile
import ufs3.kernel.fildex.Fildex.FildexFile
import ufs3.kernel.filler.Filler.FillerFile

trait Data {
  val EmptyUUID: String = "                                " //32 space string
  // UFS3 trait
  trait UFS3 {
    def blockFile: AtomicReference[BlockFile]
    def fillerFile: AtomicReference[FillerFile]
    def fildexFile: AtomicReference[FildexFile]
  }

  object UFS3 {
    private[core] def apply(_blockFile: BlockFile, _fillerFile: FillerFile, _fildexFile: FildexFile): UFS3 = new UFS3 {
      val fildexFile: AtomicReference[FildexFile] = new AtomicReference[FildexFile](_fildexFile)
      val fillerFile: AtomicReference[FillerFile] = new AtomicReference[FillerFile](_fillerFile)
      val blockFile: AtomicReference[BlockFile]   = new AtomicReference[BlockFile](_blockFile)
    }
  }

  // core config
  trait CoreConfig {
    def fillerBlockPath: Block.Path
    def fillerBlockSize: Block.Size
    def idxBlockSize: Block.Size
    def fillerReadBufferSize: Block.Size
  }
}
object Data extends Data