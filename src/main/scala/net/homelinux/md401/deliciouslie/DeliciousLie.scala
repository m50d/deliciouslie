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

sealed trait BakedCake[Layers] {
  type BurntType <: HList
}

class BakedNil extends BakedCake[HNil] {
  type BurntType = HNil
}

final case class BakedCons[A, PreviousLayers <: HList: *->*[BakedCake]#λ](a: RawLayer[PreviousLayers, A], pl: PreviousLayers) {
  type BurntType = A :: HNil
}

object BurnCake extends (BakedCake ~> Id) {
  def apply(bn: BakedNil) = Nil
  def apply[A, PreviousLayers <: HList: *->*[BakedCake]#λ](bc: BakedCons[A, PreviousLayers]): bc.BurntType = {
    val previousBurn = apply(bc.pl)
    bc.a.withLayer.f(previousBurn) :: previousBurn
  }
}

object DeliciousLie {
	type BurntLayer[A] = A
}