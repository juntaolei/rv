package rv

trait FsmError extends Throwable

case class FsmNoInitialStateError() extends FsmError

class FsmMonitor[Event]:
    thisMonitor =>

    private type Transition = PartialFunction[Event, Fsm]

    trait Fsm:
        thisFsm =>

        var initialState: Option[Fsm] = None

        private var transition: Option[Transition] = None

        /**
         * Define a step function for a finite state machine.
         * Note it only steps once.
         *
         * @param t PartialFunction[Event, Fsm]
         * @return this Fsm or new Fsm
         */
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

    // TODO: Private this variable later. Currently, expose for use with invariant
    var states: Set[Fsm] = Set()

    private var invariant: List[(String, Unit => Boolean)] = List()

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

    private def check(name: String, inv: Boolean): Unit =
        if !inv then println(s"[Violated] $name")


    /**
     * Add and check if an invariant holds when .
     *
     * @param name      Invariant name
     * @param predicate Lambda predicate for this invariant
     */
    def invariant(name: String)(predicate: => Boolean): Unit =
        invariant ::= (name, _ => predicate)
        check(name, predicate)

    def verify(event: Event): FsmMonitor[Event] =
        // Apply event to all defined states.
        states = states.map(state =>
            state(event) match
                case Some(newState) =>
                    newState
                case None => state
        )
        // Check if invariant holds.
        invariant.foreach((name, predicate) => check(s"$event: $name", predicate(())))
        // Reset all machines at final good states.
        states = states.foldLeft(Set())((acc, state) =>
            state match
                case Ok =>
                    state.initialState match
                        case Some(initialState) => acc + initialState
                        case None => acc
                case state => acc + state
        )
        thisMonitor