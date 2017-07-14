package fildex

import java.nio.ByteBuffer
import java.util.{Timer, TimerTask}

import akka.actor.{Actor, ActorSystem, Props}
import cats.Id
import ufs3.kernel.fildex.Fildex
import ufs3.kernel.fildex.Fildex.FildexFile
import ufs3.kernel.filler.Filler

/**
  * Created by songwenchao on 2017/7/14.
  */
object FildexInterpreter extends Fildex.Handler[Id] {

  private[this] var indexMap     = Map.empty[String, (Long, Long)]
  private[this] lazy val timer   = new Timer
  private[this] lazy val started = false

  private[this] lazy val system     = ActorSystem("index")
  private[this] lazy val indexActor = system.actorOf(Props[IndexActor], "indexActor")

  override def check(ff: Filler.FillerFile): Id[Fildex.FildexFile] = ???

  override def repair(ff: Filler.FillerFile): Id[Unit] = ???

  override def create(ff: Filler.FillerFile): Id[Fildex.FildexFile] = FildexFile(ff).value

  override def append(ff: Fildex.FildexFile, key: String, startPos: Long, endPos: Long): Id[Unit] = {
    indexActor ! (key, startPos, endPos)
    if (!started) {
      timer.scheduleAtFixedRate(new TimerTask {
        override def run(): Unit = indexActor ! ff
      }, 1000, 3000)
    } else ()
  }

  override def close(ff: Fildex.FildexFile): Id[Unit] = ff.close()

  private final class IndexActor extends Actor {
    override def receive: Receive = {
      case (key: String, startPos: Long, endPos: Long) ⇒
        indexMap += (key → (startPos, endPos))
      case ff: FildexFile ⇒
        var totalSize = 0L
        val buffer    = ByteBuffer.allocate(48 * 100)
        indexMap.foreach { index ⇒
          if (buffer.position() <= buffer.capacity()) {
            buffer.put(index._1.getBytes("utf-8"))
            buffer.putLong(index._2._1)
            buffer.putLong(index._2._2)
            totalSize += 48
          } else {
            ff.seekToTail()
            ff.writeData(buffer, buffer.flip().limit())
            buffer.clear()
            totalSize -= buffer.capacity()
          }
        }
        if (totalSize > 0 && buffer.position() > 0) ff.writeData(buffer, buffer.flip().limit())
        else ()
      case _ ⇒ throw new IllegalArgumentException("can't handle message,only receive tuple3 in correct style")
    }
  }
}
