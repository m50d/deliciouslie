package net.homelinux.md401.deliciouslie

import shapeless._
import UnaryTCConstraint._
import TypeOperators._
import HList._
import BasisConstraint._

trait Layer[A] {
  def withLayer[B](f: A => B): B
}

case class ContextDependent[Deps <: HList, A](f: Deps => A)

trait RawLayer[Deps <: HList, A] {
  def withLayer: ContextDependent[Deps, Layer[A]]
}

class Cake[Layers <: HList: *->*[Layer]#λ](layers: Layers) {
  def withCake[RequiredLayers <: HList, A](f: RequiredLayers => A)(implicit removeAll: RemoveAll[RequiredLayers,Layers]): A = {
    val (requiredLayers, otherLayers) = layers.removeAll[RequiredLayers]
    f(requiredLayers)
  }
  def bakeLayer[RequiredLayers <: HList, A](rawLayer: RawLayer[RequiredLayers, A])(implicit removeAll: RemoveAll[RequiredLayers,Layers]): Cake[Layer[A] :: Layers] = {
	val bakedLayer: Layer[A] = withCake(rawLayer.withLayer.f)
	new Cake(bakedLayer :: layers)
  }
}

object DeliciousLie {
}