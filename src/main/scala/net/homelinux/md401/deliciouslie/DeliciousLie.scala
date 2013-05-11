package net.homelinux.md401.deliciouslie

import shapeless._
import UnaryTCConstraint._
import TypeOperators._
import HList._
import BasisConstraint._

object DeliciousLie {
  trait BakedLayer[A] {
    def withComponent(f: A => Unit): Unit
  }

  type ContextDependent[Deps <: HList, A] = Deps => A

  trait Layer[Deps <: HList, A] {
    object callback {
      def map(g: (A => Unit) => Unit): ContextDependent[Deps, BakedLayer[A]] = deps => new BakedLayer[A] {
        def withComponent(f: A => Unit) = g(f)
      }
    }
    def context[B]()(implicit selector: Selector[Deps, B]): {
      def flatMap(g: B => ContextDependent[Deps, BakedLayer[A]]): ContextDependent[Deps, BakedLayer[A]]
      def map(g: B => A): ContextDependent[Deps, BakedLayer[A]]
    } = new Object() {
      def flatMap(g: B => ContextDependent[Deps, BakedLayer[A]]): ContextDependent[Deps, BakedLayer[A]] = { deps =>
        new BakedLayer[A] {
          def withComponent(f: A => Unit) = g(deps.select[B])(deps).withComponent(f)
        }
      }
      def map(g: B => A): ContextDependent[Deps, BakedLayer[A]] = { deps =>
        new BakedLayer[A] {
          def withComponent(f: A => Unit) = f(g(deps.select[B]))
        }
      }
    }

    val withLayer: ContextDependent[Deps, BakedLayer[A]]
  }

  trait BottomLayer[A] extends Layer[HNil, A] {
    def layer: A
    override val withLayer = { _: HNil =>
      new BakedLayer[A] {
        def withComponent(f: A => Unit) = f(layer)
      }
    }
  }

  implicit def expandContext[SmallDeps <: HList, LargeDeps <: HList, A](layer: Layer[SmallDeps, A])(implicit removeAll: RemoveAll[SmallDeps, LargeDeps]): Layer[LargeDeps, A] =
    new Layer[LargeDeps, A] {
      val withLayer = { lds: LargeDeps =>
        val (sds, _) = lds.removeAll[SmallDeps]
        layer.withLayer(sds)
      }
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
    def wit[B, LayerDeps <: HList](layer: Layer[LayerDeps, B])(implicit removeAll: RemoveAll[LayerDeps, BurntType]) = BakedCons[B, BakedCons[A, PreviousLayers]](layer, this)
  }

}