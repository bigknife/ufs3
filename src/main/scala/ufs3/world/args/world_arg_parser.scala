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

  head("ufs3", "0.1.20170817_0001")

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

  // Command.Repair
  cmd("repair")
    .text("repair: repair the index file")
    .action((_, c) ⇒ c.copy(cmd = Some(Repair), repairArg = Some(RepairArg())))
    .children(
      opt[String]("idx-size")
        .abbr("is")
        .required()
        .text("the block index file size, should end with G, M, K as the unit")
        .validate(validSize)
        .action((s, c) ⇒ c.copy(repairArg = c.repairArg.map(_.copy(idxSize = Some(s))))),
      fillerFileOpt("file", "f")((s, c) ⇒ c.copy(repairArg = c.repairArg.map(_.copy(file = s)))),
      logOpt
    )

  // Command.HttpServer
  cmd("http-server")
    .text("http-server: start a http server to expose get/put interface")
    .action((_, c) ⇒ c.copy(cmd = Some(HttpServer), httpServerArg = Some(HttpServerArg())))
    .children(
      opt[Unit]("read-write")
        .abbr("rw")
        .action((_, c) ⇒ c.copy(httpServerArg = c.httpServerArg.map(_.copy(mode = "read-write")))),
      opt[Unit]("read-only")
        .abbr("ro")
        .action((_, c) ⇒ c.copy(httpServerArg = c.httpServerArg.map(_.copy(mode = "read-only")))),
      opt[String]("host")
        .text("the host to be listened")
        .action((s, c) ⇒ c.copy(httpServerArg = c.httpServerArg.map(_.copy(host = s)))),
      opt[Int]("port")
        .text("the port to be listened")
        .action((s, c) ⇒ c.copy(httpServerArg = c.httpServerArg.map(_.copy(port = s)))),
      opt[String]("backup-target")
        .text("the backup server address, like localhost:3081")
        .abbr("bt")
        .action((s, c) ⇒ c.copy(httpServerArg = c.httpServerArg.map(_.copy(backupServer = Some(s))))),
      opt[String]("app")
        .text("the app  of ufs3 , format as appId,accessId,accessKey")
        .action((s, c) ⇒ c.copy(httpServerArg = c.httpServerArg.map(_.copy(app = Some(s))))),
      opt[String]("collie-server")
        .text("the url of collie server, format as protocol://host:port/root")
        .action((s, c) ⇒ c.copy(httpServerArg = c.httpServerArg.map(_.copy(collieServer = Some(s))))),
      opt[String]("config-param")
        .text(
          "the config param of config kit, format as env:creator:timeout:interval , timeout and interval must be number")
        .action((s, c) ⇒ c.copy(httpServerArg = c.httpServerArg.map(_.copy(collieClientConfig = Some(s))))),
      fillerFileOpt("file", "f")((s, c) ⇒ c.copy(httpServerArg = c.httpServerArg.map(_.copy(file = s)))),
      logOpt
    )

  // Command.BackupServer
  cmd("backup-server")
    .text("backup-server: start backup server")
    .action((_, c) ⇒ c.copy(cmd = Some(BackupServer), backupServerArg = Some(BackupServerArg())))
    .children(
      opt[String]("host")
        .text("the host backup server to listen on")
        .action((s, c) ⇒ c.copy(backupServerArg = c.backupServerArg.map(_.copy(host = s)))),
      opt[Int]("port")
        .text("the port backup server to listen on")
        .action((s, c) ⇒ c.copy(backupServerArg = c.backupServerArg.map(_.copy(port = s)))),
      fillerFileOpt("file", "f")((s, c) ⇒ c.copy(backupServerArg = c.backupServerArg.map(_.copy(file = s)))),
      logOpt
    )

  // Command.Backup
  cmd("backup")
    .text("backup: backup the file identified by key to the target of another UFS3")
    .action((_, c) ⇒ c.copy(cmd = Some(Backup), backupArg = Some(BackupArg())))
    .children(
      opt[String]("target")
        .required()
        .abbr("t")
        .text("target socket address of the UFS3 backup server, eg: localhost:3081")
        .action((s, c) ⇒ c.copy(backupArg = c.backupArg.map(_.copy(target = Some(s))))),
      opt[String]("key")
        .abbr("k")
        .text("the file key to be backuped")
        .action((s, c) ⇒ c.copy(backupArg = c.backupArg.map(_.copy(key = Some(s))))),
      fillerFileOpt("file", "f")((s, c) ⇒ c.copy(backupServerArg = c.backupServerArg.map(_.copy(file = s)))),
      logOpt
    )
}
