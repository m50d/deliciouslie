package net.homelinux.md401.deliciouslie

import shapeless.HList
import shapeless.Selector

trait Context[Deps <: HList] {
  def deps: Deps
  def inject[A]()(implicit selector: Selector[Deps, A]): A = deps.select[A]
}

case class ContextDependent[Deps <: HList, A](f: Context[Deps] => A)

trait Component[Deps <: HList] {
  val cake = new Object(){
    def foreach[A](f: Context[Deps] => A): ContextDependent[Deps, A] = ContextDependent(f)
  }
}