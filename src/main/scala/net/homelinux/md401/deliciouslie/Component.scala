package net.homelinux.md401.deliciouslie

import shapeless.HList

trait Context[Deps <: HList] {
  def fromContext[A]: A = null.asInstanceOf[A]
}

trait Component[Deps <: HList] {

}