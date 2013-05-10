package net.homelinux.md401.deliciouslie

import shapeless._

case class ContextDependent[Deps <: HList, A](f: Deps => A)

object Implicits {
  implicit def noContext2Function[A](cd: ContextDependent[HNil, A]): () => A
   = () => cd.f(HNil)
}

trait Component[Deps <: HList] {
  val context = new Object(){
    def foreach[A, B](f: A => B)(implicit selector: Selector[Deps, A]): ContextDependent[Deps, B] =
      ContextDependent({deps => f(deps.select[A])})
  }
}

trait ComponentImpl[A] {
  def withComponent[B, PD <: HList](f: A :: PD => B): PD => B
}