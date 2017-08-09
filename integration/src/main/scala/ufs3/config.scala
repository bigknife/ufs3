/**
  * config.scala
  * ----------
  * ufs3 uni config
  * @author bigknife
  * @since 2017/7/21
  */
package ufs3
package integration
package config

import ufs3.interpreter.block.BlockInterpreter
import ufs3.interpreter.fildex.FildexInterpreter
import ufs3.interpreter.filler.FillerInterpreter
import ufs3.interpreter.sandwich.{SandwichInInterpreter, SandwichOutInterpreter}
import ufs3.log.interpreter.LogInterpreter

trait UniConfig {
  def blockConfig: BlockInterpreter.Config   = new BlockInterpreter.Config {}
  def fillerConfig: FillerInterpreter.Config = new FillerInterpreter.Config {}
  def fildexConfig: FildexInterpreter.Config = new FildexInterpreter.Config {}
  def logConfig: LogInterpreter.Config       = LogInterpreter.config()
  def sandwichInConfig: SandwichInInterpreter.Config = new SandwichInInterpreter.Config {
    def inputBufferSize: Int = 4 * 1024 * 1024
  }
  def sanwichOutConfig: SandwichOutInterpreter.Config = new SandwichOutInterpreter.Config {
    def outputBufferSize: Long = 4 * 1024 * 1024
  }
}

object UniConfig {
  def apply(): UniConfig = new UniConfig() {}
}
