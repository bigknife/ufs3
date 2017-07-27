/**
  * main.scala
  * ----------
  * integration main
  * @author bigknife
  * @since 2017/7/21
  */
package ufs3
package integration

import java.net.InetSocketAddress

import command._
import org.apache.log4j.{ConsoleAppender, Level, Logger, PatternLayout}
import main.parser._

import scala.util.{Failure, Success}

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
        case _                                ⇒ Level.INFO
      }
      //create appender
      //configure the appender
      val PATTERN = "%d [%p|%c|%C{1}] %m%n"
      console.setLayout(new PatternLayout(PATTERN))
      console.setThreshold(level)
      console.activateOptions()
      //add appender to any Logger (here is root)
      Logger.getRootLogger.addAppender(console)
      Logger.getRootLogger.setLevel(level)
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
        PutCommand.writeLocalFile(x.coreConfig, x.putLocalFile, x.putKey) match {
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

      case Some(x) if x.cmd == "free-block" ⇒
        initLog4j(x.logLevel)
        FreeCommand.runFreeSpaceOfFillerProg(x.coreConfig, x.freeSpaceUnit) match {
          case Success(_) ⇒
          case Failure(t) ⇒
            log.debug(s"calculate free space for block failed", t)
            log.error(t.getMessage)
        }

      case Some(x) if x.cmd == "free-idx" ⇒
        initLog4j(x.logLevel)
        FreeCommand.runFreeSpaceOfFildexProg(x.coreConfig, x.freeSpaceUnit) match {
          case Success(_) ⇒
          case Failure(t) ⇒
            log.debug(s"calculate free space for index failed", t)
            log.error(t.getMessage)
        }

      case Some(x) if x.cmd == "repair" ⇒
        initLog4j(x.logLevel)
        RepairCommand.run(x.coreConfig) match {
          case Success(_) ⇒
          case Failure(t) ⇒
            log.debug(s"repair index file failed", t)
            log.error(t.getMessage)
        }

      case Some(x) if x.cmd == "http-server" ⇒
        initLog4j(x.logLevel)
        log.info(s"start http server:${x.serveHost}:${x.servePort}, ${x.serveMode}")
        ServeCommand.run(x.coreConfig, x.serveHost, x.servePort, x.serveMode)

      case Some(x) if x.cmd == "backup-server" ⇒
        initLog4j(x.logLevel)
        import scala.concurrent.ExecutionContext.Implicits.global
        BackupServerCommand.run(x.backupServerHost, x.backupServerPort) onComplete {
          case Success(_) ⇒ log.info(s"start backup server:${x.backupServerHost}:${x.backupServerPort}")
          case Failure(t) ⇒ log.error("start backup server failed", t)
        }

      case Some(x) if x.cmd == "backup" ⇒
        initLog4j(x.logLevel)
        log.info(s"backuping file identified by ${x.backupKey}")
        val s: Array[String] = x.backupTarget.get.split(":")
        val target = new InetSocketAddress(s(0), s(1).toInt)
        import scala.concurrent.ExecutionContext.Implicits.global
        BackupCommand.backup(x.coreConfig, x.backupKey, target) onComplete {
          case Success(_) ⇒ log.info(s"backuped file identified by ${x.backupKey}")
          case Failure(t) ⇒ log.error("backup failed", t)
        }


      case Some(_) ⇒ System.err.println("please re-run ufs with --help or -h: ufs3 --help")
      case _       ⇒ //println("please run with help command: ufs3 help")
    }
  }
}
