deliciouslie
============
A lightweight dependency injection pattern that understands initialization and teardown.
Inspired by the Cake Pattern.

Why?
----
 * The cake pattern is awesome, but it doesn't handle component lifecycles at all
 * Class initialization order is no good
  * Not safe - if we get it wrong, NPE at runtime
  * No way to separate "wiring up" a context from starting it

Goals
-----
 * Lightweight
  * Implemented in pure scala, without macros
  * Only dependency is Shapeless
  * Attempts to minimize syntactic overhead
 * Safe
  * Dependencies are checked at compile time
  * Service initialization happens in the correct order in the cake
  * Services are only in scope while they're live
 * Interoperable
  * Uses standard Shapeless types where possible

Usage
-----
````scala
import shapeless._
import net.homelinux.md401.deliciouslie.DeliciousLie._

//component with no dependencies or lifecycle (shortcut syntax)
object LogComponent extends BottomLayer[Log] {
  def layer() = new Log()
}
//component with lifecycle, no dependencies
object DatabaseComponent extends Layer[HNil, DatabaseConnection] {
  val withService =
    for { f <- callback } yield {
      val dbConn = setupDatabaseConnection()
      try {
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
      //compiler can usually infer types
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
//"wit" syntax is deliberately evocative of "with" as we would use in the classical cake pattern
val cake = bake wit LogComponent wit DatabaseComponent wit UserFetcherComponent wit WebserverComponent

//to actually run the lifecycle, call burn
cake.burn({ services =>
    //The full list of services is available here if we need them
    while(!shouldShutDown()) Thread.sleep(1000)
  })
````
