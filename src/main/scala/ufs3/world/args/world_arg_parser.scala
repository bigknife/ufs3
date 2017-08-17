package ufs3.world.args
import java.io.File

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

  // Command.Create
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

  // Command.Put
  cmd("put")
    .action((_, c) ⇒ c.copy(cmd = Some(Put), putArg = Some(PutArg())))
    .text("put: put a local file or some remote resource identified by url into the ufs3")
    .children(
      opt[File]("file")
        .required()
        .abbr("f")
        .text("local file path, should be a valid local file")
        .validate(x ⇒ if (x.exists() && x.isFile) Right(()) else Left(s"not a file: $x"))
        .action((f, c) ⇒ c.copy(putArg = c.putArg.map(_.copy(localFile = Some(f))))),
      opt[String]("key")
        .abbr("k")
        .text("set the key of the file in the ufs3, the key should be 32 length")
        .validate(x ⇒ if (x.length != 32) Left("key should be 32 length") else Right(()))
        .action((k, c) ⇒ c.copy(putArg = c.putArg.map(_.copy(key = Some(k))))),
      fillerFileOpt("into", "in")((s, c) ⇒ c.copy(putArg = c.putArg.map(_.copy(file = s)))),
      logOpt
    )

  // Command.Get
  cmd("get")
    .action((_, c) ⇒ c.copy(cmd = Some(Get), getArg = Some(GetArg())))
    .text("get: read file from ufs3 with key, and output to somewhere")
    .children(
      opt[String]("key")
        .abbr("k")
        .text("the file key in the ufs3")
        .action((s, c) ⇒ c.copy(getArg = c.getArg.map(_.copy(key = Some(s))))),
      opt[File]("file")
        .required()
        .abbr("f")
        .text("local file path, should be a valid local file")
        .validate(x ⇒ if (!x.exists()) Right(()) else Left(s"file $x exists, please use another not existed file path"))
        .action((f, c) ⇒ c.copy(getArg = c.getArg.map(_.copy(toSaveFile = Some(f))))),
      fillerFileOpt("out-form", "o")((s, c) ⇒ c.copy(getArg = c.getArg.map(_.copy(file = s)))),
      logOpt
    )

  // Command.List
  cmd("list")
    .action((_, c) ⇒ c.copy(cmd = Some(List), listArg = Some(ListArg())))
    .text("list: list all of the files in ufs3 instance")
    .children(
      fillerFileOpt("file", "f")((s, c) ⇒ c.copy(listArg = c.listArg.map(_.copy(file = s)))),
      logOpt
    )

  // Command.Free***Space
  cmd("free")
    .text("free: view the free space")
    .children(
      cmd("block")
        .text("block: view the free space of block")
        .action((_, c) ⇒ c.copy(cmd = Some(FreeFillerSpace), freeSpaceArg = Some(FreeSpaceArg())))
        .children(
          opt[String]("with-unit")
            .text("the unit for showing free space (G|M|K), default is M")
            .action((u, c) ⇒ c.copy(freeSpaceArg = c.freeSpaceArg.map(_.copy(unit = u)))),
          fillerFileOpt("file", "f")((s, c) ⇒ c.copy(freeSpaceArg = c.freeSpaceArg.map(_.copy(file = s)))),
          logOpt
        ),
      cmd("idx")
        .text("idx: view the free space of index")
        .action((_, c) ⇒ c.copy(cmd = Some(FreeFildexSpace), freeSpaceArg = Some(FreeSpaceArg())))
        .children(
          opt[String]("with-unit")
            .text("the unit for showing free space (G|M|K), default is M")
            .action((u, c) ⇒ c.copy(freeSpaceArg = c.freeSpaceArg.map(_.copy(unit = u)))),
          fillerFileOpt("file", "f")((s, c) ⇒ c.copy(freeSpaceArg = c.freeSpaceArg.map(_.copy(file = s)))),
          logOpt
        )
    )
}
