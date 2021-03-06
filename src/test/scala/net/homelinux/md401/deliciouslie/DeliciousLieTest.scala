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

  object rawComponent1 extends Layer[HNil, Service1] {
    val withService =
      for { f <- callback } yield {
        lifecycle.start()
        val ret = f(Service1())
        lifecycle.stop()
        ret
      }
  }
  object rawComponent2 extends Layer[Service1 :: HNil, Service2] {
    val withService = for {
      s1 <- context
    } yield {
      Service2(s1)
    }
  }

  object rawComponent3 extends Layer[Service2 :: HNil, Service3] {
    val withService = for {
      s2 <- context
      f <- callback
    } yield {
      f(Service3(s2))
    }
  }
  
  object component4 extends Layer[Service1:: HNil, String] {
    val withService = for {s1 <- context[Service1]} yield {"hello" }
  }
  
  object component5 extends BottomLayer[Int] {
    val layer = 5
  }
  
  val cake = bake wit rawComponent1 wit rawComponent2 wit rawComponent3 wit component4 wit component5

  @Test
  def instantiatesComponentExactlyOncePerBurn() {
    lifecycle.start()
    expectLastCall()
    lifecycle.stop()
    expectLastCall()
    replay(lifecycle)
    cake.burn({ l => {} })
    verify(lifecycle)
    reset(lifecycle)
    lifecycle.start()
    expectLastCall()
    lifecycle.stop()
    expectLastCall()
    replay(lifecycle)
    cake.burn({ l => {} })
    verify(lifecycle)
  }
}