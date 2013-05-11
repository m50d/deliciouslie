package net.homelinux.md401.deliciouslie

import shapeless._
import UnaryTCConstraint._
import TypeOperators._
import HList._
import BasisConstraint._
import net.homelinux.md401.deliciouslie.DeliciousLie.Layer
import net.homelinux.md401.deliciouslie.Impl.BakedNil

object DeliciousLie {
  /**
   * An instantiated component provider, potentially with a lifecycle
   */
  trait BakedLayer[A] {
    /**
     * Call f with a fully initialized A, and perform any necessary teardown afterwards
     */
    def withComponent(f: A => Unit): Unit
  }

  /**
   * Type alias to clarify intent. Something that provides an A, but expects to be instantiated
   * in the context of Deps
   */
  type ContextDependent[Deps <: HList, A] = Deps => A

  /**
   * The general case of a component: provides a service A, given the context Deps.
   * Client code should implement withLayer; context and callback are provided as syntactic sugar
   * to make implementing withLayer easier.
   */
  trait Layer[Deps <: HList, A] {
    /**
     * If client code wishes to define a lifecycle, it should use this as the *last* statement of a for/yield.
     */
    object callback {
      def map(g: (A => Unit) => Unit): ContextDependent[Deps, BakedLayer[A]] = deps => new BakedLayer[A] {
        def withComponent(f: A => Unit) = g(f)
      }
    }
    
    /**
     * Client code uses this in a for/yield when their component depends on previous components from the context
     */
    def context[B]()(implicit selector: Selector[Deps, B]): {
      def flatMap(g: B => ContextDependent[Deps, BakedLayer[A]]): ContextDependent[Deps, BakedLayer[A]]
      def map(g: B => A): ContextDependent[Deps, BakedLayer[A]]
    } = new Object() {
      def flatMap(g: B => ContextDependent[Deps, BakedLayer[A]]): ContextDependent[Deps, BakedLayer[A]] = { deps =>
        new BakedLayer[A] {
          def withComponent(f: A => Unit) = g(deps.select[B])(deps).withComponent(f)
        }
      }
      /**
       * This is called when the last step of the for/yield is from context, meaning client code does *not* want to define any lifecycle
       */
      def map(g: B => A): ContextDependent[Deps, BakedLayer[A]] = { deps =>
        new BakedLayer[A] {
          def withComponent(f: A => Unit) = f(g(deps.select[B]))
        }
      }
    }

    val withLayer: ContextDependent[Deps, BakedLayer[A]]
  }

  /**
   * Helper to make a special case simpler: a component that has no dependencies and no lifecycle
   * Client code should override layer
   */
  trait BottomLayer[A] extends Layer[HNil, A] {
    def layer: A
    override val withLayer = { _: HNil =>
      new BakedLayer[A] {
        def withComponent(f: A => Unit) = f(layer)
      }
    }
  }

  /**
   * Entry point for creating a cake. Cakes take the form "bake wit Layer1 wit Layer2 wit..."
   * A cake is a fully instantiated, but not initialized context; call burn(f) to initialize the context, run f with
   * this valid context, and then tear down the context
   */
  def bake: BakedNil = new BakedNil()
}

object Impl {
  /**
   * We can always view a component which depends on a small context as depending on a larger context
   */
  implicit def expandContext[SmallDeps <: HList, LargeDeps <: HList, A](layer: Layer[SmallDeps, A])(implicit removeAll: RemoveAll[SmallDeps, LargeDeps]): Layer[LargeDeps, A] =
    new Layer[LargeDeps, A] {
      val withLayer = { lds: LargeDeps =>
        val (sds, _) = lds.removeAll[SmallDeps]
        layer.withLayer(sds)
      }
    }

  /**
   * An assembled cake: a list of layers such that each depends only (and, for convenience, precisely) on those to the left of it
   * This is similar to a HList (and could possibly be modeled with a KList)
   */
  sealed trait BakedCake[Layers] {
    /**
     * The list of components that will be provided 
     */
    type BurntType <: HList
    def burn(f: BurntType => Unit): Unit
  }

  final case class BakedNil() extends BakedCake[HNil] {
    type BurntType = HNil
    def burn(f: HNil => Unit) = f(HNil)
    def wit[A](layer: Layer[HNil, A]) = BakedCons[A, BakedNil](layer, this)
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