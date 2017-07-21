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
import ufs3.core.CoreConfig
import ufs3.kernel.block.Block

import scala.util.{Failure, Success, Try}

/**
  * Main entrance
  */
object Main {

  case class Args(
      cmd: String = "",
      blockSize: String = "",
      idxSize: String = "",
      file: String = "./ufs3.filler",
      readBufferSize: String = "8K",
      logLevel: String = "debug"
  ) {
    def coreConfig: CoreConfig = new CoreConfig {
      import Block.Size._
      // ss: should be <number>G|M|K|B
      private[this] def str2Size(ss: String): Block.Size = {
        ss.last match {
          case 'G' ⇒ ss.substring(0, ss.length - 1).GiB
          case 'M' ⇒ ss.substring(0, ss.length - 1).MiB
          case 'K' ⇒ ss.substring(0, ss.length - 1).KiB
          case _   ⇒ throw new IllegalArgumentException("size should be end with G|M|K")
        }
      }

      def idxBlockSize: Block.Size         = str2Size(idxSize)
      def fillerReadBufferSize: Block.Size = str2Size(readBufferSize)
      def fillerBlockSize: Block.Size      = str2Size(blockSize)
      def fillerBlockPath: Block.Path      = Block.Path(file)
    }
  }
  object Args {
    val empty: Args = Args()
  }
  val progName: String = "ufs3"
  val ver: String      = "0.1"
  val log: Logger = Logger.getLogger("ufs3")

  def main(args: Array[String]): Unit = {

    def initLog4j(logLevel: String) = {
      import org.apache.log4j.Logger
      val console = new ConsoleAppender()

      val level = logLevel match {
        case x if x.equalsIgnoreCase("debug") ⇒ Level.DEBUG
        case x if x.equalsIgnoreCase("warn") ⇒ Level.WARN
        case x if x.equalsIgnoreCase("info") ⇒ Level.INFO
        case x if x.equalsIgnoreCase("fatal") ⇒ Level.FATAL
        case x if x.equalsIgnoreCase("error") ⇒ Level.ERROR
        case x if x.equalsIgnoreCase("trace") ⇒ Level.TRACE
        case _ ⇒ Level.DEBUG
      }
      //create appender
      //configure the appender
      val PATTERN = "%d [%p|%c|%C{1}] %m%n"
      console.setLayout(new PatternLayout(PATTERN))
      console.setThreshold(level)
      console.activateOptions()
      //add appender to any Logger (here is root)
      Logger.getRootLogger.addAppender(console)

      /*
      val fa = new Nothing
      fa.setName("FileLogger")
      fa.setFile("mylog.log")
      fa.setLayout(new Nothing("%d %-5p [%c{1}] %m%n"))
      fa.setThreshold(Level.DEBUG)
      fa.setAppend(true)
      fa.activateOptions


      Logger.getRootLogger.addAppender(fa)
      */
      //repeat with all other desired appenders
    }

    val validSize: String ⇒ Either[String, Unit] = x ⇒ {
      //x should end with 'G/M/K/B'
      val end   = x.endsWith("G") || x.endsWith("M") || x.endsWith("K")
      val start = x.substring(0, x.length - 1)
      Try {
        start.toDouble
      } match {
        case Success(_) ⇒
          if (end) Right(()) else Left("size is illegal, check the unit(should be G|M|K. the legal one is like: 2.5G")
        case Failure(_) ⇒ Left("size is illegal, it should start with double and end with G|M|K")
      }
    }
    val parser = new scopt.OptionParser[Args](progName) {
      head(progName, ver)
      help("help").abbr("h").text("prints this usage text")
      cmd("init")
        .action((_, c) ⇒ c.copy(cmd = "init"))
        .text("init: create a block file for ufs3")
        .children(
          opt[String]("block-size")
            .required()
            .abbr("bs")
            .text("the file block size, should end with G, M, K as the unit")
            .validate(validSize)
            .action((s, c) ⇒ c.copy(blockSize = s)),
          opt[String]("file")
            .abbr("f")
            .text("the block file path, default is current ./ufs3.filler")
            .action((s, c) ⇒ c.copy(file = s)),
          opt[String]("idx-size")
            .abbr("is")
            .required()
            .text("the block index file size, should end with G, M, K as the unit")
            .validate(validSize)
            .action((s, c) ⇒ c.copy(idxSize = s)),
          opt[String]("log-level")
            .abbr("ll")
            .text("log level: debug|info|warn|error, if not same as this, it should be the default: debug")
            .action((s, c) ⇒ c.copy(logLevel = s))
        )
    }

    parser.parse(args, Args.empty) match {
      case Some(x) if x.cmd == "init" ⇒
        initLog4j(x.logLevel)
        log.info("It will take a long time to init a ufs3 block file, please wait with your patience!")
        InitCommand.run(x.coreConfig)
        log.info(s"Congratulations! the block file is created, see ${x.coreConfig.fillerBlockPath.file.value.getAbsolutePath}")
        log.info("Now, Give more patience, wait the OS refresh the buffer to the disk, until this process exist, PLEASE DON'T send kill signal to this process")
      case Some(_)                    ⇒ System.err.println("please re-run ufs with --help or -h: ufs3 --help")
      case _                          ⇒ //println("please run with help command: ufs3 help")
    }
  }
}
