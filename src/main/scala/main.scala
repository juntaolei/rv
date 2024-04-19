import rv._

trait Event

case class acquire(threadId: Long) extends Event

case class release(threadId: Long) extends Event

class SLMonitor extends FsmMonitor[Event]:
    private var doubleAcquire: Boolean = false
    private var threadSet: Set[Long] = Set()

    property("Thread holding lock must immediately release when another thread tries to acquire") {
        !doubleAcquire
    }

    property("At most two threads can be associated with this lock") {
        threadSet.size <= 2
    }

    step {
        case acquire(tid) =>
            step {
                case acquire(tid1) if tid == tid1 =>
                    Ok
                case acquire(tid1) if tid != tid1 =>
                    step {
                        case release(tid2) if tid == tid2 =>
                            // Cleanup transition as the step transition step exactly once
                            doubleAcquire = false
                            Ok
                        case _ =>
                            doubleAcquire = true
                            // Land in error for final state so invariant will always be violated
                            Error
                    }
                case release(tid1) if tid == tid1 =>
                    Ok
            }
        case release(tid) =>
            Ok
    }

    always {
        case acquire(tid) =>
            threadSet += tid
            Ok
    }

@main
def main(): Unit = {
    val m1 = new SLMonitor
    val m2 = new SLMonitor
    val m3 = new SLMonitor
    val trace =
        """
          |cats.effect.IOFiber@6c22ddf3 WAITING
          |├ flatMap @ catseffect.examples.LiveFiberSnapshot$.<clinit>(IOAppSpec.scala:293)
          |╰ timeoutTo @ catseffect.examples.LiveFiberSnapshot$.<clinit>(IOAppSpec.scala:297)
          |cats.effect.IOFiber@6c22ddf4 WAITING
          |├ flatMap @ catseffect.examples.LiveFiberSnapshot$.<clinit>(IOAppSpec.scala:293)
          |╰ timeoutTo @ catseffect.examples.LiveFiberSnapshot$.<clinit>(IOAppSpec.scala:297)
          |""".stripMargin

    FiberCompiler(trace) match
        case Left(value) => println(value)
        case Right(value) => value.foreach(println)

    println("Simulating step to error state")
    m1.verify(acquire(1))
    m1.verify(acquire(2)) // State prior to final state transition
    m1.verify(acquire(2)) // Invariant never recover
    m1.verify(release(1))

    println("Simulating good state")
    m2.verify(acquire(1))
    m2.verify(acquire(2)) // State prior to final state transition
    m2.verify(release(1)) // Invariant recovers
    m2.verify(acquire(2))
    m2.verify(release(2))
    m2.verify(acquire(1))
    m2.verify(acquire(2))
    m2.verify(acquire(2)) // Since step is once only, invariant never gets violate again

    println("Simulating three threads error and double acquire")
    m3.verify(acquire(1))
    m3.verify(release(1))
    m3.verify(acquire(2))
    m3.verify(release(2))
    m3.verify(acquire(3)) // Inva
    m3.verify(acquire(4))
}