package ufs3

import java.util.concurrent.atomic.AtomicReference

import collie.client.CollieDescriptor
import collie.client.javadsl.{Client, CollietHolder}
import collie.domain.model.config.core.ConfigItem
import ufs3.core.data.Data.CoreConfig
import ufs3.integration.main.parser.Args
import ufs3.kernel.block.Block
import ufs3.kernel.block.Block.Size

import scala.util.{Failure, Success, Try}

/**
  * Created by songwenchao on 2017/8/7
  */
class ServerConfig(args: Args) {

  import ufs3.kernel.block.Block.Size._

  private val configRef = new AtomicReference[Vector[ConfigItem]]

  lazy val descriptor: CollieDescriptor = {
    val url                  = args.collieServer
    val array: Array[String] = url.split(":")
    require(array.length == 3, "collie server address format incorrect")
    val restArray: Array[String] = array(2).split("/")
    require(restArray.length == 2, "collie server address format incorrect")
    Try {
      CollieDescriptor(protocol = array(0),
                       host = array(1).substring(2),
                       port = restArray(0).toInt,
                       root = s"/${restArray(1)}")
    } match {
      case Success(x) ⇒ x
      case Failure(e) ⇒ throw e
    }
  }
  lazy val listener = new CollietHolder.CollietChanged {
    override def onCollietChanged(configItems: Vector[ConfigItem]): Unit = {
      println(configItems)
      configRef.set(configItems)
    }
  }
  lazy val client: Client = {
    val appArray = args.app.split(",")
    require(appArray.length == 3, "app flag must include appId accessId accessKey")
    val restArray = args.configParam.split(":")
    require(restArray.length == 4, "make sure input string include env creator timeout and interval")
    Try {
      val client = Client
        .newBuilder()
        .withDescriptor(descriptor)
        .withAppId(appArray(0))
        .withAccessId(appArray(1))
        .withAccessKey(appArray(2))
        .withEnv(restArray(0))
        .withCreator(restArray(1))
        .withTimeout(restArray(2).toLong)
        .withPollIntervalMillis(restArray(3).toLong)
        .withListener(listener)
        .build()
      configRef.set(client.getVector)
      client
    } match {
      case Success(x) ⇒ x
      case Failure(e) ⇒ throw e
    }
  }

  def coreConfig: CoreConfig = new CoreConfig {

    def strToSize(str: String): Size = {
      str.last match {
        case 'G' ⇒ str.substring(0, str.length - 1).GiB
        case 'M' ⇒ str.substring(0, str.length - 1).MiB
        case 'K' ⇒ str.substring(0, str.length - 1).KiB
        case _   ⇒ throw new IllegalArgumentException("size should end with G|M|K")
      }
    }

    override def idxBlockSize: Block.Size         = strToSize(args.idxSize)
    override def fillerReadBufferSize: Block.Size = strToSize(File.bufferSize)
    override def fillerBlockSize: Block.Size      = strToSize(args.blockSize)
    override def fillerBlockPath: Block.Path      = Block.Path(args.file)
  }

  object File {
    def bufferSize: String = client.getString(configRef.get(), "ufs3.file.buffer.size")
  }

}
