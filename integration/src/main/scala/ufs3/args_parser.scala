/**
  * args_parser.scala
  * ----------
  * parser of parsing main args
  * @author bigknife
  * @since 2017/7/22
  */
package ufs3
package integration
package main

import java.io.File

import scopt.OptionDef
import ufs3.core.data.Data._
import ufs3.kernel.block.Block

import scala.util.{Failure, Success, Try}

package object parser {
  case class Args(
      cmd: String = "",
      blockSize: String = "",
      idxSize: String = "",
      file: String = "./ufs3.filler",
      readBufferSize: String = "4M",
      logLevel: String = "info",
      putLocalFile: File = null,
      getLocalFile: File = null,
      getKey: String = null,
      listLimit: Int = -1,
      listOrder: String = "desc",
      freeSpaceUnit: String = "M",
      serveHost: String = "0.0.0.0",
      servePort: Int = 3080,
      putKey: Option[String] = None,
      backupTarget: Option[String] = None, // Some("localhost:8080")
      serveMode: String = "read-only", // or read-write
      backupServerHost: String = "0.0.0.0",
      backupServerPort: Int = 3081,
      backupKey: String = ""

  ) {
    def coreConfig: CoreConfig = new CoreConfig {
      import ufs3.kernel.block.Block.Size._
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

    def logOpt: OptionDef[String, Args] =
      opt[String]("log-level")
        .abbr("ll")
        .text("log level: debug|info|warn|error, if not same as this, it should be the default: debug")
        .action((s, c) ⇒ c.copy(logLevel = s))
    def fillerFileOpt(name: String, abbrName: String): OptionDef[String, Args] =
      opt[String](name)
        .abbr(abbrName)
        .text("the block file path, default is current ./ufs3.filler")
        .action((s, c) ⇒ c.copy(file = s))
    version("version").abbr("v").text("print version of current ufs3")
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
        fillerFileOpt("file", "f"),
        opt[String]("idx-size")
          .abbr("is")
          .required()
          .text("the block index file size, should end with G, M, K as the unit")
          .validate(validSize)
          .action((s, c) ⇒ c.copy(idxSize = s)),
        logOpt
      )

    cmd("put")
      .action((_, c) ⇒ c.copy(cmd = "put"))
      .text("put: put a local file or some remote resource identified by url into the ufs3")
      .children(
        opt[File]("file")
          .required()
          .abbr("f")
          .text("local file path, should be a valid local file")
          .validate(x ⇒ if (x.exists() && x.isFile) Right(()) else Left(s"not a file: $x"))
          .action((f, c) ⇒ c.copy(putLocalFile = f)),
        opt[String]("key")
          .abbr("k")
          .text("set the key of the file in the ufs3, the key should be 32 length")
          .action((k, c) ⇒ c.copy(putKey = Some(k))),
        fillerFileOpt("out", "o"),
        logOpt
      )

    cmd("get")
      .action((_, c) ⇒ c.copy(cmd = "get"))
      .text("get: read file from ufs3 with key, and output to somewhere")
      .children(
        opt[String]("key")
          .abbr("k")
          .text("the file key in the ufs3")
          .action((s, c) ⇒ c.copy(getKey = s)),
        opt[File]("file")
          .required()
          .abbr("f")
          .text("local file path, should be a valid local file")
          .validate(x ⇒
            if (!x.exists()) Right(()) else Left(s"file $x exists, please use another not existed file path"))
          .action((f, c) ⇒ c.copy(getLocalFile = f)),
        fillerFileOpt("in", "i"),
        logOpt
      )

    cmd("list")
      .abbr("ls")
      .action((_, c) ⇒ c.copy(cmd = "list"))
      .text("list: list the file in ufs3 instance")
      .children(
        opt[Int]("limit")
          .text("limit the count of result")
          .action((l, c) ⇒ c.copy(listLimit = l)),
        opt[String]("order")
          .text("the list result order")
          .validate(x ⇒ if (x.equals("asc") || x.equals("desc")) Right(()) else Left("order should be `asc` or `desc`"))
          .action((o, c) ⇒ c.copy(listOrder = o)),
        fillerFileOpt("file", "f"),
        logOpt
      )

    cmd("free")
      .text("free: view the free space")
      .children(
        cmd("block")
          .text("block: view the free space of block")
          .action((_, c) ⇒ c.copy(cmd = "free-block"))
          .children(
            opt[String]("with-unit")
              .text("the unit for showing free space (G|M|K), default is M")
              .action((u, c) ⇒ c.copy(freeSpaceUnit = u)),
            fillerFileOpt("file", "f"),
            logOpt
          ),
        cmd("idx")
          .text("idx: view the free space of index")
          .action((_, c) ⇒ c.copy(cmd = "free-idx"))
          .children(
            opt[String]("with-unit")
              .text("the unit for showing free space (G|M|K), default is M")
              .action((u, c) ⇒ c.copy(freeSpaceUnit = u)),
            fillerFileOpt("file", "f"),
            logOpt
          )
      )

    cmd("repair")
      .text("repair: repair the index file")
      .action((_, c) ⇒ c.copy(cmd = "repair"))
      .children(
        opt[String]("idx-size")
          .abbr("is")
          .required()
          .text("the block index file size, should end with G, M, K as the unit")
          .validate(validSize)
          .action((s, c) ⇒ c.copy(idxSize = s)),
        fillerFileOpt("file", "f"),
        logOpt
      )

    cmd("http-server")
      .text("http-serve: start a http server to expose get/put interface")
      .action((_, c) ⇒ c.copy(cmd = "http-server"))
      .children(
        opt[Unit]("read-write")
          .abbr("rw")
          .action((_, c) ⇒ c.copy(serveMode = "read-write")),
        opt[Unit]("read-only")
          .abbr("ro")
          .action((_, c) ⇒ c.copy(serveMode = "read-only")),
        opt[String]("host")
          .text("the host to be listened")
          .action((s, c) ⇒ c.copy(serveHost = s)),
        opt[Int]("port")
          .text("the port to be listened")
          .action((s, c) ⇒ c.copy(servePort = s)),
        opt[String]("backup-target")
            .text("the backup server address, like localhost:3081")
            .abbr("bt")
            .action((s, c) ⇒ c.copy(backupTarget = Some(s))),
        fillerFileOpt("file", "f"),
        logOpt
      )

    cmd("backup-server")
      .text("backup-server: start backup server")
      .action((_, c) ⇒ c.copy(cmd = "backup-server"))
      .children(
        opt[String]("host")
          .text("the host backup server to listen on")
          .action((s, c) ⇒ c.copy(backupServerHost = s)),
        opt[Int]("port")
          .text("the port backup server to listen on")
          .action((s, c) ⇒ c.copy(backupServerPort = s)),
        fillerFileOpt("file", "f"),
        logOpt
      )

    cmd("backup")
      .text("backup: backup the file identified by key to the target of another UFS3")
      .action((_, c) ⇒ c.copy(cmd = "backup"))
      .children(
        opt[String]("target")
            .required()
          .abbr("t")
          .text("target socket address of the UFS3 backup server, eg: localhost:3081")
          .action((s, c) ⇒ c.copy(backupTarget = Some(s))),
        opt[String]("key")
            .abbr("k")
            .text("the file key to be backuped")
            .action((s, c) ⇒ c.copy(backupKey = s)),
        fillerFileOpt("file", "f"),
        logOpt
      )
  }
}
