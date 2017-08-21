package ufs3.world.utils

import ufs3.kernel.commons.{Idx, Size}
import ufs3.kernel.commons.Size._

object render {
  def renderIdxs(idxs: Vector[Idx])(renderer: String ⇒ Unit): Unit = {
    val rowTuple =
      ("key", "uuid", "size", "start", "end") +:
        idxs.map(x ⇒ (x.key, x.uuid, x.fileLength.B.toStringWithUnit("M"), x.startPoint.toString, x.endPoint.toString))

    //find max length
    val maxLengthTuples =
      rowTuple.foldLeft((0, 0, 0, 0, 0)) { (acc, n) ⇒
        (Math.max(acc._1, n._1.length),
         Math.max(acc._2, n._2.length),
         Math.max(acc._3, n._3.length),
         Math.max(acc._4, n._4.length),
         Math.max(acc._5, n._5.length))
      }

    def row(x: (String, String, String, String, String)): String =
      s"%-${maxLengthTuples._1}s ".format(x._1) +
        s"%-${maxLengthTuples._2}s ".format(x._2) +
        s"%-${maxLengthTuples._3}s ".format(x._3) +
        s"%${maxLengthTuples._4}s ".format(x._4) +
        s"%-${maxLengthTuples._5}s ".format(x._5)

    val s = rowTuple.map(row).mkString("\r\n")
    renderer(s)
  }

  def renderSize(l: Long, u: String)(renderer: String ⇒ Unit): Unit = {
    import Size._
    renderer(l.B.toStringWithUnit(u))
  }
}
