/**
  * fildex.scala
  * ----------
  * Filler File Index
  * @author bigknife
  * @since 2017/7/13
  */
package ufs3
package kernel
package fildex

import filler._
import scala.language.higherKinds
import scala.language.implicitConversions
import sop._
trait Fildex[F[_]] {
  def check(ff: FillerFile): Par[F, Boolean]
  def repair(ff: FillerFile): Par[F, FildexFile]
  def create(ff: FillerFile): Par[F, FildexFile]
  def append(ff: FildexFile, key: String, startPos: Long, endPos: Long): Par[F, Unit]
}

trait FildexFile
trait Data