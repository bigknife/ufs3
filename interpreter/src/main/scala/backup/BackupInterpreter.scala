package backup

import cats.Id
import ufs3.kernel.backup.Backup

/**
  * Created by songwenchao on 2017/7/14.
  */
object BackupInterpreter extends Backup.Handler[Id] {
  override protected[this] def open(): Id[Unit] = ???

  override protected[this] def close(): Id[Unit] = ???

  override protected[this] def send(data: Backup.Data): Id[Unit] = ???
}
