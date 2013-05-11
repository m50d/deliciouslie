package net.homelinux.md401.deliciouslie

import net.homelinux.md401.deliciouslie.DeliciousLie._
import shapeless._

class DeliciousLieTest {
  case class Service1()
  case class Service2(s1: Service1)
  case class Service3(s2: Service2)
  trait Service1Lifecycle {
    def start(): Unit
    def stop(): Unit
  }
  
  val bakedNil = new BakedNil()
  val rawComponent1 = new RawLayer[HNil, Service1] {
    val withLayer = {
      hn: HNil =>
        new Layer[Service1](){
          def withLayer[A](f: Service1 => A) = {
            f(Service1())
          }
        }
    }
  }
  val bakedComponent1 = BakedCons[Service1, BakedNil](rawComponent1, bakedNil)
  val rawComponent2 = new RawLayer[Service1 :: HNil, Service2] {
    val withLayer = {
      l : (Service1 :: HNil) =>
        new Layer[Service2](){
          def withLayer[A](f: Service2 => A) = {
            f(Service2(l.head))
          }
        }
    }
  }
  val bakedComponent2 = BakedCons[Service2, BakedCons[Service1, BakedNil]](rawComponent2, bakedComponent1)
}