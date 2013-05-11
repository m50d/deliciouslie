package net.homelinux.md401.deliciouslie

import shapeless._
//import scalaz._
//import Scalaz._

case class ContextDependent[Deps <: HList, A](f: Deps => A)

object Implicits {
  implicit def noContext2Function[A](cd: ContextDependent[HNil, A]): () => A = () => cd.f(HNil)
  implicit def function2NoContext[A](f: () => A): ContextDependent[HNil, A] = ContextDependent({ HNil => f() })
  //  implicit def contextContext2BiggerContext[Deps1<: HList, Deps2 <: HList, A](cd: ContextDependent[Deps1, ContextDependent[Deps2, A]])
  //  	(implicit prepend: Prepend[Deps1, Deps2]): ContextDependent[prepend.Out, A] =
  //    ContextDependent({deps: prepend.Out => cd.f(deps.take(Deps1.length)).f(deps.drop(Deps1.length))})
  def instantiate[A](component: Component[A, HNil]) = {}
  //  class ContextDependentMonad[Deps <: HList] extends Monad[({ type cd[A] = ContextDependent[Deps, A] })#cd] {
  //    def point[A](f: => A) = ContextDependent({ deps => f })
  //    def bind[A, B](fa: ContextDependent[Deps, A])(f: A => ContextDependent[Deps, B]) =
  //      ContextDependent({ deps: Deps =>
  //        f(fa.f(deps)).f(deps)
  //      })
  //  }
  //  implicit def contextDependentMonad[Deps <: HList] = new ContextDependentMonad[Deps]
  def applyContext[B, OtherDeps <: HList, A](cd: ContextDependent[B :: OtherDeps, A], b: B): ContextDependent[OtherDeps, A] =
    ContextDependent({ od: OtherDeps => cd.f(b :: od) })
}

import Implicits._

trait Component[A, Deps <: HList] {
  //What client code should implement
  def component[B](f: A => B): ContextDependent[Deps, B]

  def inject[B]()(implicit selector: Selector[Deps, B]): ContextDependent[Deps, B] =
    ContextDependent({ _.select[B] })
}

//Possibly unnecessary, but here for clarity
trait LeafComponent[A] extends Component[A, HNil] {
  //What client code should implement
  def leafComponent[B](f: A => B): () => B
  override def component[B](f: A => B): ContextDependent[HNil, B] = leafComponent(f)
}

sealed trait Context[Singletons <: HList]

sealed class EmptyContext extends Context[HNil] {}

final case class ConsContext[H, T <: Context[_]]()

import TypeOperators._
import HList._
import UnaryTCConstraint._
abstract class Cake[L <: HList: *->*[LeafComponent]#É…](l: L) {
  //Client code should implement
  //Will be called with the real cake
  def run(): ContextDependent[L, Unit]

  object extractComponent extends (LeafComponent ~> ({ type l[A] = ContextDependent[HNil, A] })#l) {
    def apply[A](lc: LeafComponent[A]): ContextDependent[HNil, A] = lc.component(identity)
  }
  //Call this to build the context and run the run
  def bake()(implicit mapper: Mapper[extractComponent.type, L]) = {
    val components = l map extractComponent
    //	components.foldLeft(new EmptyContext())(null)
    //    run.f()
  }
}

import shapeless._

object ShapelessTest {
  case class MyHolder[A](a: A)
  object extractA extends (MyHolder ~> Option) {
    def apply[A](mh: MyHolder[A]): Option[A] = Some(mh.a)
  }
  def unpackList[L <: HList: *->*[MyHolder]#É…](l: L)(implicit mapper: shapeless.Mapper[net.homelinux.md401.deliciouslie.ShapelessTest.extractA.type, L]) =
    l map extractA
  unpackList(MyHolder("foo") :: MyHolder(5) :: HNil)
}