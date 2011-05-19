fun orGate(a1, a2, output) =
   let fun orAction() =
       let val newValue = getSignal(a1) orelse getSignal(a2);
       in
          afterDelay(orGateDelay,
                     fn () => setSignal(output, newValue))
       end;
   in
     (orAction(a1, orAction);
      orAction(a2, orAction))
   end;

fun addToAgenda(time, action, Agenda(currTime, segments)) = 
    let
	fun belongsBefore([]) = true
	  | belongsBefore(Seg(t, evs)::rest) = time < t
	fun addToSegments(Seg(t, evs)::rest) = 
	    if t = time
	    then (evs := !evs @ [action]; Seg(t, evs)::rest;)
	    else if belongsBefore(rest)
	    then Seg(t, evs)::Seg(time, ref [action])::rest
	    else Seg(t, evs)::addToSegments(rest);
    in
	if belongsBefore(!segments)
	then segments := Seg(time, ref [action])::!segments
	else segments := addToSegments(!segments)
    end;
