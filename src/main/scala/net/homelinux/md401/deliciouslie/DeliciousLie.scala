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
  trait BakedLayer[A] {
    def withComponent(f: A => Unit): Unit
  }

  type ContextDependent[Deps <: HList, A] = Deps => A

  trait Layer[Deps <: HList, A] {
    def withLayer: ContextDependent[Deps, BakedLayer[A]]
  }

  sealed trait BakedCake[Layers] {
    type BurntType <: HList
    def burn(f: BurntType => Unit): Unit
  }

  final case class BakedNil() extends BakedCake[HNil] {
    type BurntType = HNil
    def burn(f: HNil => Unit) = f(HNil)
    def wit[A](layer: Layer[BurntType, A]) = BakedCons[A, BakedNil](layer, this)
  }

  final case class BakedCons[A, PreviousLayers <: BakedCake[_]](a: Layer[PreviousLayers#BurntType, A], pl: PreviousLayers) extends BakedCake[A :: PreviousLayers#BurntType] {
    type BurntType = A :: PreviousLayers#BurntType
    def burn(f: A :: PreviousLayers#BurntType => Unit) = {
      pl.burn({ plb: PreviousLayers#BurntType =>
        a.withLayer(plb).withComponent({
          al => f(al :: plb)
        })
      })
    }
    def wit[B](layer: Layer[BurntType, B]) = BakedCons[B, BakedCons[A, PreviousLayers]](layer, this)
  }
  
}