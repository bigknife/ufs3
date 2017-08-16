package ufs3.world.args
import scopt._
import ufs3.world.commons.Command._
import ufs3.world.commons._

import scala.util.{Failure, Success, Try}

object parser extends OptionParser[Args]("ufs3") {
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

  def logOpt: OptionDef[String, Args] =
    opt[String]("log-level")
      .abbr("ll")
      .text("log level: debug|info|warn|error, if not same as this, it should be the default: debug")
      .action((s, c) ⇒ c.copy(logLevel = LogLevel(s)))

  def fillerFileOpt(name: String, abbrName: String)(action: (String, Args) ⇒ Args): OptionDef[String, Args] =
    opt[String](name)
      .abbr(abbrName)
      .text("the block file path, default is current ./ufs3.filler")
      .action((s, c) ⇒ action(s, c))

  version("version").abbr("v").text("print version of current ufs3")

  head("ufs3", "0.1.20170815_0001")

  help("help").abbr("h").text("prints this usage text")

  cmd("create")
    .action((_, c) ⇒ c.copy(cmd = Some(Create), createArg = Some(CreateArg())))
    .text("create: create a block file for ufs3")
    .children(
      opt[String]("block-size")
        .required()
        .abbr("bs")
        .text("the file block size, should end with G, M, K as the unit")
        .validate(validSize)
        .action((s, c) ⇒ c.copy(createArg = c.createArg.map(_.copy(blockSize = s)))),
      fillerFileOpt("file", "f")(action = (s, c) ⇒ c.copy(createArg = c.createArg.map(_.copy(file = s)))),
      opt[String]("idx-size")
        .abbr("is")
        .required()
        .text("the block index file size, should end with G, M, K as the unit")
        .validate(validSize)
        .action((s, c) ⇒ c.copy(createArg = c.createArg.map(_.copy(idxSize = s)))),
      logOpt
    )
}