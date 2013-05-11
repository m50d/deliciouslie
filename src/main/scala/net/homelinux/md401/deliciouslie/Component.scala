package net.homelinux.md401.deliciouslie

import shapeless._
import scalaz._
import Scalaz._

case class ContextDependent[Deps <: HList, A](f: Deps => A)

object Implicits {
  implicit def noContext2Function[A](cd: ContextDependent[HNil, A]): () => A
   = () => cd.f(HNil)
  implicit def function2NoContext[A](f: () => A): ContextDependent[HNil, A] = ContextDependent({HNil => f()})
//  implicit def contextContext2BiggerContext[Deps1<: HList, Deps2 <: HList, A](cd: ContextDependent[Deps1, ContextDependent[Deps2, A]])
//  	(implicit prepend: Prepend[Deps1, Deps2]): ContextDependent[prepend.Out, A] =
//    ContextDependent({deps: prepend.Out => cd.f(deps.take(Deps1.length)).f(deps.drop(Deps1.length))})
  def instantiate[A](component: ComponentImpl[A, HNil]) = {}
  class ContextDependentMonad[Deps <: HList] extends Monad[({type cd[A] = ContextDependent[Deps, A]})#cd] {
    def point[A](f: => A) = ContextDependent({deps => f})
    def bind[A, B](fa: ContextDependent[Deps, A])(f: A => ContextDependent[Deps, B]) =
      ContextDependent({deps: Deps => 
        f(fa.f(deps)).f(deps)})
  }
}

trait Component[Deps <: HList] {
  val context = new Object(){
    def foreach[A, B](f: A => B)(implicit selector: Selector[Deps, A]): ContextDependent[Deps, B] =
      ContextDependent({deps => f(deps.select[A])})
  }
}

trait ComponentImpl[A, Deps <: HList] {
  //What client code should implement
  def component[B](f: A => B): Deps => B
//  def withComponent[B, PD <: HList](f: A :: PD => B): PD => B
}

abstract class Cake[Deps <: HList]() {
  //Client code should implement
  //Will be called with the real cake
  def run(): ContextDependent[Deps, Unit]
  
  //Call this to build the context and run the run
  def start() = {
    
  }
}