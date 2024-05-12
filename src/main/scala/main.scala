import rv.*
import cats.effect.{IO, IOApp}

trait Event

case class get(res: Int) extends Event

case class complete(res: Boolean) extends Event

case class threadStatus(id: String, status: String) extends Event

/**
 * Partial deadlock dump example from https://typelevel.org/cats-effect/docs/core/fiber-dumps
 *
 * import cats.effect.{IO, IOApp}
 *
 * object Deadlock extends IOApp.Simple {
 * val run =
 * for {
 * latch <- IO.deferred[Unit]
 *
 * body = latch.get
 * fiber <- body.start
 * _ <- fiber.join
 *
 * _ <- latch.complete(())
 * } yield ()
 * }
 * */
val sampleDeferredDeadlock: String =
    """
      |cats.effect.IOFiber@56824a14 WAITING
      | ├ flatMap @ Deadlock$.$anonfun$run$2(Deadlock.scala:26)
      | ├ flatMap @ Deadlock$.$anonfun$run$1(Deadlock.scala:25)
      | ├ deferred @ Deadlock$.<clinit>(Deadlock.scala:22)
      | ├ flatMap @ Deadlock$.<clinit>(Deadlock.scala:22)
      | ╰ run$ @ Deadlock$.run(Deadlock.scala:19)
      |
      |cats.effect.IOFiber@6194c61c WAITING
      | ├ get @ Deadlock$.$anonfun$run$1(Deadlock.scala:24)
      | ╰ get @ Deadlock$.$anonfun$run$1(Deadlock.scala:24)
      |""".stripMargin

class DeadlockMonitor(totalThreads: Int) extends FsmMonitor[Event] {
    debug = Debug.Warn

    private var numWaiting: Set[String] = Set()

    property("If all threads are WAITING, then there is a deadlock.") {
        numWaiting.size < totalThreads
    }

    always {
        case threadStatus(id, status) =>
            if !numWaiting.contains(id) && status == "WAITING" then numWaiting = numWaiting ++ Set(id)
            if (numWaiting.size < totalThreads) {
                Ok
            } else {
                Error
            }
    }
}

class DeferredIntMonitor extends FsmMonitor[Event] {
    debug = Debug.Warn

    private var completedIntReturn = true

    private var completedInt: Option[Int] = None
    private var completedIntIsCorrect = true

    private var completed = false
    private var alwaysFalseAfterCompletion = true

    property("get on an empty Deferred will block until the Deferred is completed") {
        completedIntReturn
    }

    property("get on a completed Deferred will always immediately return its content") {
        completedIntIsCorrect
    }

    property("complete(a) on a Deferred that has already been completed will not modify its content, and will result false") {
        alwaysFalseAfterCompletion
    }

    always {
        case get(res) =>
            if (!completed) {
                completedIntReturn = false
                println("Must complete before Deferred return some value")
                Error
            } else if (completed && (completedInt.isEmpty || completedInt.contains(res))) {
                if (completedInt.isEmpty) completedInt = Some(res)
                println("get on Deferred returns some consistent Int")
                Ok
            } else if (completed && completedInt.isDefined && !completedInt.contains(res)) {
                println("get on Deferred does not return some consistent Int")
                completedIntIsCorrect = false
                Error
            } else {
                Error
            }
        case complete(res) if !completed =>
            println("Deferred has just been completed")
            completed = true
            Ok
        case complete(res) if completed && !res =>
            println("Deferred has been completed and correctly result false now")
            Ok
        case complete(res) if completed && res =>
            println("Deferred has been completed but completed result true instead")
            alwaysFalseAfterCompletion = false
            Error
    }
}

object Main extends IOApp.Simple {
    private def deadlock() =
        FiberCompiler(sampleDeferredDeadlock) match
            case Left(value) => throw new RuntimeException(value.toString)
            case Right(value) =>
                val threadSets = value.foldLeft(Set())((acc, ele) => {
                    ele match
                        case FiberThreadCall(id, status, function, context) =>
                            acc ++ Set(id)
                })
                val monitor = DeadlockMonitor(threadSets.size)
                value.map {
                    case FiberThreadCall(id, status, function, context) =>
                        monitor.verify(threadStatus(id, status))
                }

    private def property1() =
        // Property 1: complete(a) on a Deferred that has already been completed will not modify its content, and will result false
        val monitor = DeferredIntMonitor()
        for {
            latch <- IO.deferred[Int]
            firstComplete <- latch.complete(1).flatMap(res => {
                monitor.verify(complete(res))
                IO.pure(())
            })
            secondComplete <- latch.complete(1).flatMap(res => {
                monitor.verify(complete(res))
                IO.pure(())
            })
        } yield ()

    private def property2() =
        // Property 2: get on a completed Deferred will always immediately return its content
        val monitor = DeferredIntMonitor()
        for {
            latch <- IO.deferred[Int]
            firstComplete <- latch.complete(1).flatMap(res => {
                monitor.verify(complete(res))
                IO.pure(())
            })
            firstGet <- latch.get.flatMap(res => {
                monitor.verify(get(res))
                IO.pure(res)
            })
            secondGet <- latch.get.flatMap(res => {
                monitor.verify(get(res))
                IO.pure(res)
            })
        } yield ()

    private def property3() =
        // Property 3: get on an empty Deferred will block until the Deferred is completed
        val monitor = DeferredIntMonitor()
        for {
            latch <- IO.deferred[Int]
            firstGet <- IO(latch.get.flatMap(res => {
                monitor.verify(get(res))
                IO.pure(res)
            }))
            firstComplete <- IO(latch.complete(1).flatMap(res => {
                monitor.verify(complete(res))
                IO.pure(())
            }))
            secondGet <- IO(latch.get.flatMap(res => {
                monitor.verify(get(res))
                IO.pure(res)
            }))
        } yield ()

    val run: IO[Unit] =
        deadlock()
        property1()
            .flatMap(_ => property2())
            .flatMap(_ => property3())
}