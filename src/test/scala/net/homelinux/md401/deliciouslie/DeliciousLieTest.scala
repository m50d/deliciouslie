package net.homelinux.md401.deliciouslie

import net.homelinux.md401.deliciouslie.DeliciousLie._
import shapeless._
import org.easymock.EasyMock._
import org.junit.Test

class DeliciousLieTest {
  case class Service1()
  case class Service2(s1: Service1)
  case class Service3(s2: Service2)
  trait Service1Lifecycle {
    def start(): Unit
    def stop(): Unit
  }
  val lifecycle = createMock(classOf[Service1Lifecycle])
  
  val bakedNil = new BakedNil()
  val rawComponent1 = new RawLayer[HNil, Service1] {
    val withLayer = {
      hn: HNil =>
        new Layer[Service1](){
          def withLayer[A](f: Service1 => A) = {
            lifecycle.start()
            val r = f(Service1())
            lifecycle.stop()
            r
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
  
  val rawComponent3 = new RawLayer[Service2 :: Service1 :: HNil, Service3] {
    val withLayer = {
      l : (Service2 :: Service1 :: HNil) =>
        new Layer[Service3](){
          def withLayer[A](f: Service3 => A) = {
            f(Service3(l.head))
          }
        }
    }
  }
  val bakedComponent3 = BakedCons[Service3, BakedCons[Service2, BakedCons[Service1, BakedNil]]](rawComponent3, bakedComponent2)
  
  @Test
  def instantiatesComponentExactlyOnce() {
    lifecycle.start()
    expectLastCall()
    lifecycle.stop()
    expectLastCall()
    replay(lifecycle)
    bakedComponent3.burn({l => {}})
    verify(lifecycle)
  }
}