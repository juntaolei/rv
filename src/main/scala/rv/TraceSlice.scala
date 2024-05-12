package rv

case class Event(id: Long, obj: Object)

case class Parameter(id: Long, name: String, obj: Object)

case class TraceableEvent(event: Event, parameters: Option[Seq[Parameter]])

class TraceSlice:
    thisSlicer =>

    def process(traces: Seq[TraceableEvent]): Seq[(Seq[Parameter], Seq[Event])] =
        var mapT: Map[Seq[Parameter], Seq[Event]] = Map((Seq(), Seq()))
        var setTheta: Set[Parameter] = Set()


        traces.foreach(pair => {
            pair.parameters match
                case None =>
                    mapT = mapT.map((k, v) => if k.isEmpty then (k, v.appended(pair.event)) else (k, v))
                case Some(parameters) =>
                    val related = mapT.filter((k, _) => k.contains(parameters) || parameters.toSet.subsetOf(k.toSet))

                    if related.nonEmpty then
                        val updatedRelated = related.map((k, v) => (k, v.appended(pair.event)))
                        mapT = mapT ++ updatedRelated
                    else
                        mapT = mapT ++ mapT
                            .filter((k, v) => k.forall(ele => parameters.forall(parameter => ele.name != parameter.name)))
                            .map((k, v) => (k ++ parameters, v.appended(pair.event)))
        })

        mapT.toList