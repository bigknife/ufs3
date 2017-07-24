/**
  * main.scala
  * ----------
  * integration main
  * @author bigknife
  * @since 2017/7/21
  */
package ufs3
package integration

import command._
import org.apache.log4j.{ConsoleAppender, Level, Logger, PatternLayout}
import main.parser._

import scala.util.{Success, Failure}

/**
  * Main entrance
  */
object Main {

  val log: Logger = Logger.getLogger("ufs3")

  def main(args: Array[String]): Unit = {

    def initLog4j(logLevel: String) = {
      import org.apache.log4j.Logger
      val console = new ConsoleAppender()

      val level = logLevel match {
        case x if x.equalsIgnoreCase("debug") ⇒ Level.DEBUG
        case x if x.equalsIgnoreCase("warn")  ⇒ Level.WARN
        case x if x.equalsIgnoreCase("info")  ⇒ Level.INFO
        case x if x.equalsIgnoreCase("fatal") ⇒ Level.FATAL
        case x if x.equalsIgnoreCase("error") ⇒ Level.ERROR
        case x if x.equalsIgnoreCase("trace") ⇒ Level.TRACE
        case _                                ⇒ Level.DEBUG
      }
      //create appender
      //configure the appender
      val PATTERN = "%d [%p|%c|%C{1}] %m%n"
      console.setLayout(new PatternLayout(PATTERN))
      console.setThreshold(level)
      console.activateOptions()
      //add appender to any Logger (here is root)
      Logger.getRootLogger.addAppender(console)
      //repeat with all other desired appenders
    }

    parser.parse(args, Args.empty) match {
      case Some(x) if x.cmd == "init" ⇒
        initLog4j(x.logLevel)
        log.info("It will take a long time to init a ufs3 block file, please wait with your patience!")
        InitCommand.run(x.coreConfig) match {
          case Success(_) ⇒
            log.info(
              s"Congratulations! the block file is created, see ${x.coreConfig.fillerBlockPath.file.value.getAbsolutePath}")
            log.info(
              "Now, Give more patience, wait the OS refresh the buffer to the disk, until this process exist, PLEASE DON'T send kill signal to this process")
          case Failure(t) ⇒
            log.debug(s"ufs3 init failed", t)
            log.error(t.getMessage)
        }

      case Some(x) if x.cmd == "put" ⇒
        initLog4j(x.logLevel)
        log.info(s"putting file:${x.putLocalFile}")
        val now = System.currentTimeMillis()
        PutCommand.writeLocalFile(x.coreConfig, x.putLocalFile) match {
          case Success(key) ⇒
            log.info(
              s"put file:${x.putLocalFile}, the key in ufs3 is $key, time spent: ${System.currentTimeMillis() - now}ms")
          case Failure(t) ⇒
            log.debug(s"put file:${x.putLocalFile} failed", t)
            log.error(t.getMessage)
        }

      case Some(x) if x.cmd == "get" ⇒
        initLog4j(x.logLevel)
        log.info(s"getting file of key: ${x.getKey}")
        val now = System.currentTimeMillis()
        GetCommand.getToLocalFile(x.coreConfig, x.getKey, x.getLocalFile) match {
          case Success(key) ⇒
            log.info(s"saved file to ${x.getLocalFile}, time spent: ${System.currentTimeMillis() - now}ms")
          case Failure(t) ⇒
            log.debug(s"get file:${x.putLocalFile} failed", t)
            log.error(t.getMessage)
        }

      case Some(x) if x.cmd == "list" ⇒
        initLog4j(x.logLevel)
        ListCommand.run(x.coreConfig, x.listLimit, x.listOrder) match {
          case Success(_) ⇒
          case Failure(t) ⇒
            log.debug(s"list file failed", t)
            log.error(t.getMessage)
        }


      case Some(_) ⇒ System.err.println("please re-run ufs with --help or -h: ufs3 --help")
      case _       ⇒ //println("please run with help command: ufs3 help")
    }
  }
}
