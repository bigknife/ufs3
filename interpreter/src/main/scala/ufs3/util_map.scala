/**
  * util_map.scala
  * ----------
  * provide atomic map operation
  * @author bigknife
  * @since 2017/7/17
  */
package ufs3
package interpreter
package util

import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec

sealed trait AtomicMap[K, V] {
  def +=(kv: (K, V)): Unit
  def -=(k: K): Unit
  def apply(k: K): Option[V]
}

object AtomicMap {
  private[this] class AtomicMap_[K, V]() extends AtomicMap[K, V] {self ⇒
    private[this] val atom = new AtomicReference[Map[K, V]](Map.empty)

    @tailrec
    final def +=(kv: (K, V)): Unit = {
      val old = atom.get()
      val newOne = old + kv
      if (!atom.compareAndSet(old, newOne)) self.+=(kv)
    }
    @tailrec
    final def -=(k: K): Unit = {
      val old = atom.get()
      val newOne = old - k
      if (!atom.compareAndSet(old, newOne)) self.-=(k)
    }

    def apply(k: K): Option[V] = atom.get().get(k)
  }

  def apply[K, V]: AtomicMap[K, V] = new AtomicMap_[K, V]
}