package io.npj.scheme.cat

object Cons {
  def +:[A](a: A)(as: Seq[A]): Seq[A] = a +: as
}
