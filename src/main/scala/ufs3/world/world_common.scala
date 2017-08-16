package ufs3.world

import ufs3.kernel.commons.{Config, Path, Size}

object commons {
  case class CreateArg(file: String = "./ufs3.filler",
                       blockSize: String = "1G",
                       idxSize: String = "10M",
                       readBuffferSize: String = "8K") {
    private[this] def str2Size(ss: String): Size = {
      import Size._
      ss.last match {
        case 'G' ⇒ ss.substring(0, ss.length - 1).GiB
        case 'M' ⇒ ss.substring(0, ss.length - 1).MiB
        case 'K' ⇒ ss.substring(0, ss.length - 1).KiB
        case _   ⇒ throw new IllegalArgumentException("size should be end with G|M|K")
      }
    }

    def asConfig: Config = Config(Path(file), str2Size(blockSize), str2Size(idxSize), str2Size(readBuffferSize))
  }
  case class Args(
      cmd: Option[Command] = None,
      logLevel: LogLevel = LogLevel.INFO,
      createArg: Option[CreateArg] = None
  )

  sealed trait Command
  object Command {
    final case object Create extends Command
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
