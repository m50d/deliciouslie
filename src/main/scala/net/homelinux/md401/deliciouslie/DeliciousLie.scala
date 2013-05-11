package net.homelinux.md401.deliciouslie

import shapeless._
import UnaryTCConstraint._
import TypeOperators._
import HList._
import BasisConstraint._

object DeliciousLie {
	
}

trait Layer[A] {
  def withLayer[B](f: A => B): B
}

class Cake[Layers <: HList: *->*[Layer]#λ](layers: Layers) {
  def withCake[RequiredLayers <: HList : Basis[Layers]#λ, B](f: RequiredLayers => B): B = {
    null.asInstanceOf[B]
  }
}