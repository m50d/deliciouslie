package net.homelinux.md401.deliciouslie

import shapeless._
import UnaryTCConstraint._
import TypeOperators._
import HList._
import BasisConstraint._

//class Cake[Layers <: HList: *->*[Layer]#λ](layers: Layers) {
//  def withCake[RequiredLayers <: HList, A](f: RequiredLayers => A)(implicit removeAll: RemoveAll[RequiredLayers, Layers]): A = {
//    val (requiredLayers, otherLayers) = layers.removeAll[RequiredLayers]
//    f(requiredLayers)
//  }
//  def bakeLayer[RequiredLayers <: HList, A](rawLayer: RawLayer[RequiredLayers, A])(implicit removeAll: RemoveAll[RequiredLayers, Layers]): Cake[Layer[A] :: Layers] = {
//    val bakedLayer: Layer[A] = withCake(rawLayer.withLayer.f)
//    new Cake(bakedLayer :: layers)
//  }
//}

object DeliciousLie {
  type BurntLayer[A] = A

  trait Layer[A] {
    def withLayer[B](f: A => B): B
  }

  type ContextDependent[Deps <: HList, A] = Deps => A

  trait RawLayer[Deps <: HList, A] {
    def withLayer: ContextDependent[Deps, Layer[A]]
  }

  sealed trait BakedCake[Layers] {
    type BurntType <: HList
    def burn[A](f: BurntType => A): A
  }

  class BakedNil extends BakedCake[HNil] {
    type BurntType = HNil
    def burn[A](f: HNil => A) = f(HNil)
  }

  final case class BakedCons[A, PreviousLayers <: BakedCake[_]](a: RawLayer[PreviousLayers#BurntType, A], pl: PreviousLayers) extends BakedCake[A :: PreviousLayers#BurntType] {
    type BurntType = A :: PreviousLayers#BurntType
    def burn[B](f: A :: PreviousLayers#BurntType => B) = {
      pl.burn({ plb: PreviousLayers#BurntType =>
        a.withLayer(plb).withLayer({
          al => f(al :: plb)
        })
      })
    }
  }
}