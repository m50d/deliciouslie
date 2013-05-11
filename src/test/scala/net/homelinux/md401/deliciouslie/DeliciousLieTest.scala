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
  
  val bakedNil = BakedNil()
  val rawComponent1 = new Layer[HNil, Service1] {
    val withLayer = {
      hn: HNil =>
        new BakedLayer[Service1](){
          def withComponent(f: Service1 => Unit) = {
            lifecycle.start()
            f(Service1())
            lifecycle.stop()
          }
        }
    }
  }
  val rawComponent2 = new Layer[Service1 :: HNil, Service2] {
    val withLayer = {
      l : (Service1 :: HNil) =>
        new BakedLayer[Service2](){
          def withComponent(f: Service2 => Unit) = {
            f(Service2(l.head))
          }
        }
    }
  }
  
  val rawComponent3 = new Layer[Service2 :: Service1 :: HNil, Service3] {
    val withLayer = {
      l : (Service2 :: Service1 :: HNil) =>
        new BakedLayer[Service3](){
          def withComponent(f: Service3 => Unit) = {
            f(Service3(l.head))
          }
        }
    }
  }
  val cake = bakedNil wit rawComponent1 wit rawComponent2 wit rawComponent3
  
  @Test
  def instantiatesComponentExactlyOnce() {
    lifecycle.start()
    expectLastCall()
    lifecycle.stop()
    expectLastCall()
    replay(lifecycle)
    cake.burn({l => {}})
    verify(lifecycle)
  }
}