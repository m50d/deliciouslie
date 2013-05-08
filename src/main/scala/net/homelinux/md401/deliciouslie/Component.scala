package net.homelinux.md401.deliciouslie

import shapeless.HList
import shapeless.Selector

trait Context[Deps <: HList] {
  def deps: Deps
  def fromContext[A]()(implicit selector: Selector[Deps, A]): A = deps.select[A]
}

trait Component[Deps <: HList] {

}