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
   * An instantiated component that knows how to provide a service (potentially involving some kind of lifecycle)
   */
  trait BakedLayer[A] {
    /**
     * Call f with a fully initialized service, and perform any necessary teardown afterwards
     */
    def withService(f: A => Unit): Unit
  }

  /**
   * Type alias to clarify intent. Something that provides an A, but expects to be instantiated
   * in the context of Deps
   */
  type ContextDependent[Deps <: HList, A] = Deps => A

  /**
   * A general component: provides a service A, given the context Deps.
   * Client code should implement withService; context and callback are provided as syntactic sugar
   * to make this easier.
   */
  trait Layer[Deps <: HList, A] {
    /**
     * If client code wishes to define a lifecycle, it should use this as the *last* statement of a for/yield.
     */
    object callback {
      def map(g: (A => Unit) => Unit): ContextDependent[Deps, BakedLayer[A]] = deps => new BakedLayer[A] {
        def withService(f: A => Unit) = g(f)
      }
    }

    /**
     * Client code uses this in a for/yield when their service depends on previous services from the context
     */
    def context[B]()(implicit selector: Selector[Deps, B]): {
      def flatMap(g: B => ContextDependent[Deps, BakedLayer[A]]): ContextDependent[Deps, BakedLayer[A]]
      def map(g: B => A): ContextDependent[Deps, BakedLayer[A]]
    } = new Object() {
      def flatMap(g: B => ContextDependent[Deps, BakedLayer[A]]): ContextDependent[Deps, BakedLayer[A]] = { deps =>
        new BakedLayer[A] {
          def withService(f: A => Unit) = g(deps.select[B])(deps).withService(f)
        }
      }
      /**
       * This is called when the last step of the for/yield is from context, meaning client code does *not* want to define any lifecycle
       */
      def map(g: B => A): ContextDependent[Deps, BakedLayer[A]] = { deps =>
        new BakedLayer[A] {
          def withService(f: A => Unit) = f(g(deps.select[B]))
        }
      }
    }

    /**
     * The implementation of this normally looks like "for {dependency <- context; ...} yield {new A(dependency)}"
     */
    val withService: ContextDependent[Deps, BakedLayer[A]]
  }

  /**
   * A common, simpler special case: a component that has no dependencies and no lifecycle
   * Client code should override layer()
   */
  trait BottomLayer[A] extends Layer[HNil, A] {
    def layer: A
    override val withService = { _: HNil =>
      new BakedLayer[A] {
        def withService(f: A => Unit) = f(layer)
      }
    }
  }
  
  /**
   * Even simpler and more specialized case: a component that is just a fixed value
   */
  case class ValueLayer[A](layer: A) extends BottomLayer[A]

  /**
   * Entry point for creating a cake. Cakes take the form "bake wit Layer1 wit Layer2 wit..."
   * A cake is a fully instantiated, but not initialized context; call burn(f) to initialize the context, run f with
   * this valid context, and then tear down the context
   */
  def bake: BakedNil = new BakedNil()
}

object Impl {
  /**
   * We can always view a component which depends on a small context as depending on a larger context (as long as the larger context contains all the services
   * that the smaller one does)
   */
  implicit def expandContext[SmallDeps <: HList, LargeDeps <: HList, A](layer: Layer[SmallDeps, A])(implicit removeAll: RemoveAll[SmallDeps, LargeDeps]): Layer[LargeDeps, A] =
    new Layer[LargeDeps, A] {
      val withService = { lds: LargeDeps =>
        val (sds, _) = lds.removeAll[SmallDeps]
        layer.withService(sds)
      }
    }

  /**
   * An assembled cake: a list of layers such that each depends only (and, for convenience, precisely) on those to the left of it
   * This is similar to a HList (and could possibly be modeled with a KList, but I found the type constraints impractical to express)
   */
  sealed trait BakedCake[Layers] {
    /**
     * The services provided by the cake from this layer down
     */
    type BurntType <: HList

    /**
     * Run the whole cake, including any lifecycle: initialize all services, call f with this list of services, then perform any necessary teardown
     * Should be reusable (i.e. it's possible to call burn several times on the same cake)
     */
    def burn(f: BurntType => Unit): Unit
  }

  /**
   * A cake with at least one layer
   */
  final case class BakedCons[A, PreviousLayers <: BakedCake[_]](a: Layer[PreviousLayers#BurntType, A], pl: PreviousLayers) extends BakedCake[A :: PreviousLayers#BurntType] {
    type BurntType = A :: PreviousLayers#BurntType
    def burn(f: A :: PreviousLayers#BurntType => Unit) = {
      pl.burn({ plb: PreviousLayers#BurntType =>
        a.withService(plb).withService({
          al => f(al :: plb)
        })
      })
    }
    
    /**
     * Add another layer on top of this cake. removeAll enforces that the dependencies of the new layer are a subset of the dependencies from this layer down 
     */
    def wit[B, LayerDeps <: HList](layer: Layer[LayerDeps, B])(implicit removeAll: RemoveAll[LayerDeps, BurntType]) = BakedCons[B, BakedCons[A, PreviousLayers]](layer, this)
  }

  /**
   * The base case; a cake with no layers
   */
  final case class BakedNil() extends BakedCake[HNil] {
    type BurntType = HNil
    def burn(f: HNil => Unit) = f(HNil)

    /**
     * Since this cake has no layers, the first layer must have no dependencies (i.e. depends on HNil)
     */
    def wit[A](layer: Layer[HNil, A]) = BakedCons[A, BakedNil](layer, this)
  }

}