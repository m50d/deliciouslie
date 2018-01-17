# deliciouslie

A lightweight dependency injection pattern that understands initialization and teardown.
Inspired by the Cake Pattern.

## Why?

* The cake pattern is awesome, but it doesn't handle component lifecycles at all
* Class initialization order is no good
  * Not safe - if we get it wrong, NPE at runtime
  * No way to separate "wiring up" a context from "running" it

## Features

- a
  - b


    c
- d

* Lightweight
  * Implemented in pure scala, without macros
  * Only dependency is Shapeless
  * Does require somewhat longer component implementations than "classic" cake pattern
  * However, doesn't require distinct interface/implementation declarations for each component
* Safe
  * Dependencies are checked at compile time
  * Service initialization happens in the correct order in the cake
  * Services are only in scope while they're live
* Interoperable
  * Uses standard Shapeless types where possible

## Usage

````scala
import shapeless._
import net.homelinux.md401.deliciouslie.DeliciousLie._

//component with no dependencies or lifecycle (shortcut syntax)
//a component is essentially a "service factory" rather than the service itself
//hence why we're declaring our components as objects
object LogComponent extends BottomLayer[Log] {
  def layer() = new Log()
}
//component with lifecycle, no dependencies
object DatabaseComponent extends Layer[HNil, DatabaseConnection] {
  val withService =
    for { f <- callback } yield {
      val dbConn = setupDatabaseConnection()
      try {
        //dbConn never escapes this scope except through f, thus
        //should always be in the correct state anywhere it's in scope
        f(dbConn)
      } finally {
        dbConn.tearDown()
      }
    }
}

//component with dependencies, no lifecycle
object UserFetcherComponent extends Layer[Log :: DatabaseConnection :: HNil, UserFetcher] {
  val withService = for {
      //type-safe, will only compile because Log is present in the above declaration
      log <- context[Log]
      //compiler can usually infer types from how services are used inside the yield block
      databaseConnection <- context
    } yield { new UserFetcher(databaseConnection, log) }

//component with dependencies and lifecycle
object WebserverComponent extends Layer[Log :: HNil, Webserver] {
  val withService = for {
      log <- context
      //callback has to come last in the dependency list if being used
      f <- callback
    } yield {
        log.info("Starting up...")
        s = new WebServer(...)
        f(s)
        shutdownWebserver(s)
      }
}

//instantiating a cake - this will not call our init code defined in the yield blocks
//compiler enforces safety; e.g. LogComponent must come before UserFetcherComponent or WebserverComponent
//observe that unlike in the classical cake pattern circular dependencies are impossible
//"wit" syntax is deliberately evocative of "with" as used in the classical cake pattern
val cake = bake wit LogComponent wit DatabaseComponent wit UserFetcherComponent wit WebserverComponent

//to actually run the lifecycle, call burn
cake.burn({
    //We can access the services as a HList if we need to
    services =>
      while(!shouldShutDown()) Thread.sleep(1000)
  })

//burn can be called repeatedly if we want to keep the same wiring but run the setup and teardown again each time
//e.g. for a per-request context in a web server
````
