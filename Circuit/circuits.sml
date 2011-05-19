fun b2s(true) = "1"
  | b2s(false) = "0";

datatype wire = Wire of (unit -> bool) * (bool -> unit) * ((unit -> unit) -> unit);

fun callEach([]) = ()
  | callEach(f::rest) = (f(); callEach(rest));

fun makeWire() =
  let val signalValue = ref false;
      val actionListeners = ref [];
      fun setMySignal(newValue) =
         if newValue <> !signalValue
         then (signalValue := newValue;
               callEach(!actionListeners))
         else ();
      fun acceptActionListener(listener) = 
         (actionListeners := listener:: !actionListeners;
          listener());
  in
    Wire(fn () => !signalValue, setMySignal, acceptActionListener)
  end;

fun getSignal(Wire(gs, _, _)) = gs();

fun setSignal(Wire(_, ss, _), v) = ss(v);

fun addAction(Wire(_,_,aal), ac) = aal(ac);

datatype segment = Seg of int * (unit -> unit) list ref;

datatype agenda = Agenda of int ref * segment list ref;

fun makeAgenda() = Agenda(ref 0, ref []);

fun currentTime(Agenda(currTime,segments)) = !currTime;

fun setCurrentTime(Agenda(currTime,segments), nct) = currTime := nct;

fun emptyAgenda(Agenda(currTime,segments)) = !segments = [];

fun addToAgenda(time, action, Agenda(currTime,segments)) =
   let
     fun belongsBefore([]) = true
       | belongsBefore(Seg(t,evs)::rest) = time < t;
     fun addToSegments(Seg(t,evs)::rest) =
          if t = time
          then (evs := !evs@[action];
                Seg(t,evs)::rest)
          else if belongsBefore(rest)
               then  Seg(t,evs)::Seg(time, ref [action])::rest
               else Seg(t,evs)::addToSegments(rest);
   in
     if belongsBefore(!segments)
     then segments := Seg(time, ref[action]):: !segments
     else segments := addToSegments(!segments)
   end;

fun removeFirstAgendaItem(Agenda(currTime,
                                 segments as ref (Seg(time,evs)::rest))) =
    case !evs of
        [] => ()  (* shouldn't happen *)
      | ev::[] => segments := rest
      | ev::restEv => segments := Seg(time,ref restEv)::rest;

fun firstAgendaItem(Agenda(currTime,
                           ref (Seg(time,evs)::rest))) = 
   (currTime := time;
    hd(!evs));     

val theAgenda = makeAgenda();
val inverterDelay = 2;
val andGateDelay = 3;
val orGateDelay = 5;

fun afterDelay(delay, action) =
  addToAgenda(delay + currentTime(theAgenda), action, theAgenda);

fun propagate() =
   if emptyAgenda(theAgenda) then ()
   else let val firstItem = firstAgendaItem(theAgenda);
        in
          (firstItem(); removeFirstAgendaItem(theAgenda); propagate())
        end;

fun probe(name, w) =
  addAction(w, fn () => print(name ^ " " ^ 
                              Int.toString(currentTime(theAgenda)) ^
                              " New value = " ^ b2s(getSignal(w)) ^"\n"));

fun inverter(input, output) =
   let fun invertInput() =
       let val newValue = not (getSignal(input));
       in
          afterDelay(inverterDelay,
                     fn () => setSignal(output, newValue))
       end;
   in
     addAction(input, invertInput)
   end;

fun andGate(a1, a2, output) =
   let fun andAction() =
       let val newValue = getSignal(a1) andalso getSignal(a2);
       in
          afterDelay(andGateDelay,
                     fn () => setSignal(output, newValue))
       end;
   in
     (addAction(a1, andAction);
      addAction(a2, andAction))
   end;


fun orGate(a1, a2, output) =
   let fun orAction() =
       let val newValue = getSignal(a1) orelse getSignal(a2);
       in
          afterDelay(orGateDelay,
                     fn () => setSignal(output, newValue))
       end;
   in
     orAction()
   end;



fun halfAdder(a, b, s, c)  =
   let val d = makeWire();
       val e = makeWire();
   in
     (orGate(a, b, d);
      andGate(a, b, c);
      inverter(c, e);
      andGate(d, e, s))
   end;

fun fullAdder(a, b, cIn, sum, cOut) =
   let val s = makeWire();
       val c1 = makeWire();
       val c2 = makeWire();
   in 
     (halfAdder(b, cIn, s, c1);
      halfAdder(a, s, sum, c2);
      orGate(c1, c2, cOut))
   end;

fun rippleCarryAdder(Alist, Blist, Slist) = 
	let
		val cIn = makeWire();
		setSignal(cIn, false);
		


	in
		
	end;


fun add(a, b) = 
	let
		
		fun reverse([]) = []
		  | reverse(a::rest) = reverse(rest)@[a];
		fun intToBits(a) = 
			let
				fun convert(1) = [true]
				  | convert(0) = [false]
				  | convert(a) = if a mod 2 = 0
					then convert(a div 2)@[false]
					else convert(a div 2)@[true];
				fun reverse([]) = []
				  | reverse(a::rest) = reverse(rest)@[a];
			in
				reverse(convert(a))
			end;
		fun intToBitsReg(a) = 
			let
				fun convert(1) = [true]
				  | convert(0) = [false]
				  | convert(a) = if a mod 2 = 0
					then convert(a div 2)@[false]
					else convert(a div 2)@[true];
			in
				convert(a)
			end;
		fun bitsToInt(a) = 
			let
				val pos = ref ~1;
				fun power(a, 0) = 1
				  | power(a, 1) = a
				  | power(a, b) = a * power(a, b-1);
				fun bitToInt(false) = 0
				  | bitToInt(true) = 1;
				fun convert([]) = 0
				  | convert(true::rest) = 
				(pos := !pos + 1;power(2, !pos) + convert(rest))
				  | convert(false::rest) = 
				(pos := !pos + 1;convert(rest));
			in
				convert(a)
			end;

		fun bitsToWires([]) = []
		  | bitsToWires(b::rest) = 
			let
				fun bitToWire() = 
					let
						val w = makeWire();
					in
					(setSignal(w, b);
					w)
					end;
			in
				bitToWire()::bitsToWires(rest)
			end;
	in
			(* Convert to Wires *)
		bitsToInt(rippleCarryAdder(bitsToWires(intToBitsReg(a)), bitsToWires(intToBitsReg(b))))
	end;











