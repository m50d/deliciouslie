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
    val withLayer =
      for { f <- callback } yield {
        lifecycle.start()
        f(Service1())
        lifecycle.stop()
      }
  }
  val rawComponent2 = new Layer[Service1 :: HNil, Service2] {
    val withLayer = for {
      s1 <- context[Service1]
      f <- callback
    } yield {
      f(Service2(s1))
    }
  }

  val rawComponent3 = new Layer[Service2 :: HNil, Service3] {
    val withLayer = for {
      s2 <- context[Service2]
      f <- callback
    } yield {
    	f(Service3(s2))
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
    cake.burn({ l => {} })
    verify(lifecycle)
  }
}