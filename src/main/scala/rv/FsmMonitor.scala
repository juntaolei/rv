package rv

trait FsmError extends Throwable

case class FsmNoInitialStateError(string: String) extends FsmError

enum Debug:
    case NoDebug
    case Warn
    case Strict

class FsmMonitor[Event]:
    thisMonitor =>

    private type Transition = PartialFunction[Event, Fsm]

    trait Fsm:
        thisFsm =>

        var initialState: Option[Fsm] = None

        private var transition: Option[Transition] = None

        def step(t: Transition): Fsm =
            transition match
                case Some(t) =>
                    new Fsm {
                        this.step(t)
                    }
                case None =>
                    transition = Some(t)
                    thisFsm

        def always(t: Transition): Fsm =
            transition match
                case Some(t) => new Fsm {
                    this.always(t)
                }
                case None =>
                    transition = Some(t andThen (_ => thisFsm)) // Makes self looping transitions
                    thisFsm

        def next(t: Transition): Fsm =
            transition match
                case Some(t) => new Fsm {
                    this.next(t)
                }
                case None =>
                    transition = Some(t orElse (_ => thisFsm)) // Makes self looping transitions
                    thisFsm

        def until(t: Transition): Fsm = new Fsm {
            override def step(t1: Transition): Fsm =
                transition match
                    case Some(t) => new Fsm {
                        thisMonitor.until(t).step(t1)
                    }
                    case None =>
                        transition = Some(t orElse (t1 andThen (_ => thisFsm)))
                        thisFsm
        }

        inline def apply(event: Event): Option[Fsm] =
            initialState match
                case Some(_) =>
                case None => initialState = Some(thisFsm)
            transition match
                case Some(t) =>
                    if !t.isDefinedAt(event) then None
                    else
                        val nextState = t(event)
                        Some(nextState)
                case _ => None

    case object Ok extends Fsm

    case object Error extends Fsm

    var debug: Debug = Debug.NoDebug
    private var states: Set[Fsm] = Set()
    private var properties: List[(String, Unit => Boolean)] = List()

    def step(t: Transition): Fsm =
        val newFsm = new Fsm {
            this.step(t)
        }
        states += newFsm
        newFsm

    def always(t: Transition): Fsm =
        val newFsm = new Fsm {
            this.always(t)
        }
        states += newFsm
        newFsm

    def next(t: Transition): Fsm =
        val newFsm = new Fsm {
            this.next(t)
        }
        states += newFsm
        newFsm

    def until(t: Transition): Fsm =
        val newFsm = new Fsm {
            this.until(t)
        }
        states += newFsm
        newFsm

    private def check(name: String, inv: Boolean): Unit =
        debug match
            case Debug.NoDebug =>
                if !inv then println(s"[Violated] $name")
            case _ =>
                println(s"Checking $name: [${if inv then "PASSED" else "FAILED"}]")


    /**
     * Add and check if an property holds when .
     *
     * @param name      Invariant name
     * @param predicate Lambda predicate for this property
     */
    def property(name: String)(predicate: => Boolean): Unit =
        properties ::= (name, _ => predicate)
        check(name, predicate)

    def verify(event: Event): FsmMonitor[Event] =
        // Apply event to all defined states.
        states = states.map(state =>
            state(event) match
                case Some(newState) =>
                    newState
                case None => state
        )
        // Check if property holds.
        properties.foreach((name, predicate) => check(s"$event: $name", predicate(())))
        // Reset all machines at final good states.
        states = states.foldLeft(Set())((acc, state) =>
            state match
                case Ok =>
                    state.initialState match
                        case Some(initialState) => acc + initialState
                        case None =>
                            debug match
                                case Debug.NoDebug =>
                                case Debug.Warn =>
                                case Debug.Strict => throw FsmNoInitialStateError(s"No initial state for $state")
                            acc
                case state => acc + state
        )
        thisMonitor