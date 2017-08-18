package ufs3.world

import java.io.File

import ufs3.kernel.commons.{Config, Path, Size}

object commons {
  private[this] def str2Size(ss: String): Size = {
    import Size._
    ss.last match {
      case 'G' ⇒ ss.substring(0, ss.length - 1).GiB
      case 'M' ⇒ ss.substring(0, ss.length - 1).MiB
      case 'K' ⇒ ss.substring(0, ss.length - 1).KiB
      case _   ⇒ throw new IllegalArgumentException("size should be end with G|M|K")
    }
  }

  case class CreateArg(file: String = "./ufs3.filler",
                       blockSize: String = "1G",
                       idxSize: String = "10M",
                       readBuffferSize: String = "8K") {

    def asConfig: Config = Config(Path(file), str2Size(blockSize), str2Size(idxSize), str2Size(readBuffferSize))
  }

  case class PutArg(
      file: String = "./ufs3.filler",
      localFile: Option[File] = None,
      key: Option[String] = None,
      bufferSize: Option[Int] = Some(8192)
  ) {
    import Size._
    def asConfig: Config =
      Config(fillerBlockPath = Path(file), fillerReadBufferSize = bufferSize.map(_.B).getOrElse(8.KiB))
  }

  case class GetArg(
      file: String = "./ufs3.filler",
      key: Option[String] = None,
      toSaveFile: Option[File] = None,
      bufferSize: Option[Int] = Some(8192)
  ) {
    import Size._
    def asConfig: Config =
      Config(fillerBlockPath = Path(file), fillerReadBufferSize = bufferSize.map(_.B).getOrElse(8.KiB))
  }

  case class ListArg(
      file: String = "./ufs3.filler",
      toSaveFile: Option[File] = None,
      bufferSize: Option[Int] = Some(8192)
  ) {
    import Size._
    def asConfig: Config =
      Config(fillerBlockPath = Path(file), fillerReadBufferSize = bufferSize.map(_.B).getOrElse(8.KiB))
  }

  case class FreeSpaceArg(
      file: String = "./ufs3.filler",
      unit: String = "M"
  ) {
    def asConfig: Config =
      Config(fillerBlockPath = Path(file))
  }

  case class HttpServerArg(
      file: String = "./ufs3.filler",
      mode: String = "read-only",
      host: String = "localhost",
      port: Int = 3080,
      backupServer: Option[String] = None,
      app: Option[String] = None,
      collieServer: Option[String] = None,
      collieClientConfig: Option[String] = None,
      downloadBufferSize: String = "8K"
  ) {
    def asConfig: Config =
      Config(fillerBlockPath = Path(file), fillerReadBufferSize = str2Size(downloadBufferSize))
  }

  case class BackupServerArg(
      file: String = "./ufs3.filler",
      host: String = "localhost",
      port: Int = 3081,
      bufferSize: String = "8K"
  ) {
    def asConfig: Config =
      Config(fillerBlockPath = Path(file), fillerReadBufferSize = str2Size(bufferSize))
  }

  case class RepairArg(file: String = "./ufs3.filler", idxSize: Option[String] = None) {
    def asConfig: Config =
      Config(fillerBlockPath = Path(file), idxBlockSize = str2Size(idxSize.get))
  }

  case class BackupArg(
      file: String = "./ufs3.filler",
      key: Option[String] = None,
      target: Option[String] = None,
      bufferSize: String = "8K"
  ) {
    def asConfig: Config =
      Config(fillerBlockPath = Path(file), fillerReadBufferSize = str2Size(bufferSize))
  }

  case class Args(
      cmd: Option[Command] = None,
      logLevel: LogLevel = LogLevel.INFO,
      createArg: Option[CreateArg] = None,
      putArg: Option[PutArg] = None,
      getArg: Option[GetArg] = None,
      listArg: Option[ListArg] = None,
      freeSpaceArg: Option[FreeSpaceArg] = None,
      repairArg: Option[RepairArg] = None,
      httpServerArg: Option[HttpServerArg] = None,
      backupServerArg: Option[BackupServerArg] = None,
      backupArg: Option[BackupArg] = None
  )

  sealed trait Command
  object Command {
    final case object Create          extends Command
    final case object Put             extends Command
    final case object Get             extends Command
    final case object List            extends Command
    final case object FreeFildexSpace extends Command
    final case object FreeFillerSpace extends Command
    final case object Repair          extends Command
    final case object HttpServer      extends Command
    final case object BackupServer    extends Command
    final case object Backup          extends Command
  }

  sealed trait LogLevel
  object LogLevel {
    final case object DEBUG extends LogLevel
    final case object INFO  extends LogLevel
    final case object WARN  extends LogLevel
    final case object ERROR extends LogLevel

    def apply(s: String): LogLevel = s match {
      case x if x.equalsIgnoreCase("debug") ⇒ DEBUG
      case x if x.equalsIgnoreCase("warn")  ⇒ WARN
      case x if x.equalsIgnoreCase("error") ⇒ ERROR
      case _                                ⇒ INFO
    }
  }

}
