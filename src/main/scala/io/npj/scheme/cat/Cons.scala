package io.npj.scheme.cat

object Cons {
  def cons[A](a: A)(as: Seq[A]): Seq[A] = a +: as
}
