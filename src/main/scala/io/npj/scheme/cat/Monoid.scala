package io.npj.scheme.cat

trait Monoid[A] {
  def empty: A
  def append(a1: A)(a2: A): A
}

object Monoid {
  def apply[A](implicit m: Monoid[A]): Monoid[A] = m

  object syntax {
    implicit class MonoidOps[A: Monoid](self: A) {
      def <>(a: A): A = Monoid[A].append(self)(a)
    }
  }

  implicit object StringMonoid extends Monoid[String] {
    def empty: String = ""
    def append(a1: String)(a2: String): String = a1 ++ a2
  }
}
