datatype ('k, 'v)table = Table of ('k * 'v ref) list ref;

fun hasAssoc(key, []) = false
  | hasAssoc(key, (ke, va)::rest) = key = ke orelse hasAssoc(key, rest);

fun assoc(key, (a,b)::rest) =
    if key = a then (a, b) else assoc(key, rest);

fun contains(key, Table(records)) = hasAssoc(key, !records);

fun lookup(key, Table(records)) =  !(#2(assoc(key, !records)));

fun insert(key, value, tab as Table(records)) =
    if contains(key, tab)
    then let val (k, v) = assoc(key, !records) in v := value end
    else records := (key, ref value) :: (!records); 

fun makeTable(kernel) =
    let val tab = Table(ref kernel) in
        (fn (k) => contains(k, tab),
         fn (k) => lookup(k, tab),
         fn (k, v) => insert(k, v, tab))
    end;

val (has, get, put) = makeTable([]:(string * int ref) list);

fun fibby(n) = if contains(n, tab) then #2(lookup(n,tab)) else (insert(n, fibby(n-1) + fibby(n-2)); lookup(n, tab));

fun memoizedFib(n) = 
  let
      val (has, get, put) = makeTable([]:(int *int ref) list);
      fun mfib(0) = 0
        | mfib(1) = 1
        | mfib(n) = if has(n) then get(n) else
                                  let val x = mfib(n-2) + mfib(n-1);
                                  in (put(n, x ); x) end;
  in
    mfib(n)
  end;

fun memoize(f) =
    let val (has, get, put) = makeTable([]) in
    fn (x) => if has(x)
              then get(x)
                   else let val y = f(x) in (put(x, y); y) end
    end;

fun fib(0) = 1
  | fib(1) = 1
  | fib(n) = fib(n-1) + fib(n-2);

val memFib = memoize(fib);

val memoFib =
    memoize(fn (n) => if n = 0 then 1
                                    else if n = 1 then 1
                                    else memoFib(n-1) + memoFib(n-2));

val rec memoFib =
    memoize(fn (n) => if n = 0 then 1
                                    else if n = 1 then 1
                                    else memoFib(n-1) + memoFib(n-2));

val memofib =
    let
        fun fib(0, f) = 1
          | fib(1, f) = 1
          | fib(n, f) = f(n-1) + f(n-2);
      val mfib = memoize(fib);
    in
        fn (n) => mfib(n, mfib)
    end;

val memofib =
    let
        val f = ref (fn(n) => 0);
        fun fib(0) = 1
          | fib(1) = 1
          | fib(n) = !f(n-1) + !f(n-2);
    in
        (f := memoize(fib);
        !f)
    end;
