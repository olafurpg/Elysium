package nz.daved.elysium

package object misc {
  def meta[T](thunk: => T): T = thunk
  val inline = 1
}
