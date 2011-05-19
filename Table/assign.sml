
(******* Problem 1 ******)
1.
fun memoizedExp(x, y) = 
	let
		val (has, get, put) = makeTable([]:((int * int * int) * int ref) list);

		fun e(a, b, 0) = a
		  | e(a, b, n) = 
			if has((a, b, n)) 
			then get((a, b, n)) 
			else
				let
					val x = if n mod 2 = 0
					then
						e(a, b*b, n div 2)
					else
						e(a * b, b, n - 1);
				in
						(put((a, b, 0), x); x)
				end;
			
	in
		e(1, x, y)
	end;



fun memoExp(x, y) =
    let
        val f = ref (fn(n) => 0);
        fun e((a, b, 0)) = a
          | e((a, b, n)) = if n mod 2 = 0 
                      then e((a, b*b, n div 2))
                      else e((a * b, b, n - 1));
    in
        memoize(e)(1, x, y)
    end;




(******* Problem 2 ******)
2.

fun memoizedSum(f, b, n) = 
	let
		val (has, get, put) = makeTable([]:(int * int ref) list);
		fun memSum(f, b, n) = 
			if b > n
			then 0
			else
				if has(b)
				then get(b)
				else
					let 
						val x = f(b) + memSum(f, b+1, n);
					in
						(put(b+1, x); x)
					end;	
	in
		memSum(f, b, n)
	end;



fun memoSum(f, b, n) = 
	let
		fun summation(c) = if c > n 
                         then 0 
                         else f(c) + summation(c+1);
	in
		memoize(summation)(b)
	end;




(******* Problem 3 ******)
3.

fun max(L) = 
	let
		fun maximum(b, []) = b
		    | maximum(b, i::rest) = if i > b then maximum(i, rest) else maximum(b, rest);
	in
		maximum(hd(L), tl(L))
	end;
	
fun getIndex(a, b::rest) = if a = b then 1 else 1 + getIndex(a, rest);

fun knapsack((item as (weight, value))::rest, capacity) = 
	if weight < capacity then getIndex(item)::knapsack(rest, capacity-weight)
		(*fun max(a, []) = a
		  | max([], a) = a
		  | max(a, b::rest) = if a > b then max(a, rest) else max(b, rest);*)

fun map(f, r) = accumulate(fn (x, y) => f(x)::y, [], r);

val itemList = [(7, 20), (12, 3), (3, 6)];

fun knapsackProblem(items, capacity) = 
	let
		fun getIndex(a, b::rest) = if a = b then 1 else 1 + getIndex(a, rest);

		fun getItem(i, f::L) = if i = 1 then f else getItem(i-1, L);

		fun maxItem(a, b) = if a > b then a else b;

		fun totalValue([]) = 0
		  | totalValue(a::rest) = #2(getItem(a, items)) + totalValue(rest);

		fun totalWeight([]) = 0
		  | totalWeight(a::rest) = #1(getItem(a, items)) + totalWeight(rest);

		fun knapsack([], capacity, currentItems) = currentItems
		  | knapsack((item as (weight, value))::rest, capacity, currentItems) = 
			let
				val this = if weight < capacity then
					knapsack(items, capacity-weight, getIndex(item, items)::currentItems)
					else [];
				val others = knapsack(rest, capacity, currentItems)
			in
				if maxItem(totalValue(this), totalValue(others)) = totalValue(this)
				then this
				else others
			end;
	in
		knapsack(items, capacity, [])
	end;








		fun totalValue([]) = 0
		  | totalValue(a::rest) = #2(getItem(a, items)) + totalValue(rest);

		fun totalWeight([]) = 0
		  | totalWeight(a::rest) = #1(getItem(a, items)) + totalWeight(rest);


//////////////// Working non-memoized version //////////////////

datatype ('k, 'v)table = Table of ('k * 'v ref) list ref;

fun hasAssoc(key, []) = false
  | hasAssoc(key, (ke, va)::rest) = key = ke orelse hasAssoc(key, rest);

fun assoc(key, (a,b)::rest) =
    if key = a then (a, b) else assoc(key, rest);

fun contains(key, Table(records)) = hasAssoc(key, !records);

fun lookup(key, Table(records)) =  !(#2(assoc(key, !records)));



fun makeTable(kernel) =
    let val tab = Table(ref kernel) in
        (fn (k) => contains(k, tab),
         fn (k) => lookup(k, tab),
         fn (k, v) => insert(k, v, tab))
    end;



(********** Alternate Non-Memoized Version *********)
fun knapsackProblem(capacity, items) = 
	let
		fun getIndex(a, b::rest) = if a = b then 1 else 1 + getIndex(a, rest);

		fun getItem(i, f::L) = if i = 1 then f else getItem(i-1, L);

		fun maxItem(a, b) = if a > b then a else b;

		fun knapsack([], capacity, currentItems, currentValue) = (currentValue, currentItems)
		  | knapsack((item as (weight, value))::rest, capacity, currentItems, currentValue) = 
			let
				val this = if weight < capacity then
					knapsack(items, capacity-weight, getIndex(item, items)::currentItems, value + currentValue)
					else (0, []);
				val others = knapsack(rest, capacity, currentItems, currentValue)
			in
				if maxItem(#1(this), #1(others)) = #1(this)
				then this
				else others
			end;
	in
		knapsack(items, capacity, [], 0)
	end;



(************* Memoized Version ***********)

(***** Requires special insert function that returns value back ********)
fun insert(key, value, tab as Table(records)) =
    (if contains(key, tab)
    then let val (k, v) = assoc(key, !records) in v := value end
    else records := (key, ref value) :: (!records); value);

fun knapsackProblem(capacity, items) = 
	let
		val (has, get, put) = makeTable([]:(int * (int * int list) ref) list);

		fun getIndex(a, b::rest) = if a = b then 1 else 1 + getIndex(a, rest);

		fun getItem(i, f::L) = if i = 1 then f else getItem(i-1, L);

		fun maxItem(a, b) = if a > b then a else b;

		fun knapsack([], capacity, currentItems, currentValue) = (currentValue, currentItems)
		  | knapsack((item as (weight, value))::rest, capacity, currentItems, currentValue) = 
			let
				val this = if has(capacity-weight) then get(capacity-weight) else
					(if weight < capacity then
					put(capacity-weight, knapsack(items, capacity-weight, getIndex(item, items)::currentItems, value + currentValue))
					else (0, []));
				val others = if has(capacity) then get(capacity) else put(capacity, knapsack(rest, capacity, currentItems, currentValue));
			in
				if maxItem(#1(this), #1(others)) = #1(this)
				then this
				else others
			end;
	in
		knapsack(items, capacity, [], 0)
	end;





    * A(0, j) = 0
    * A(i, 0) = 0
    * A(i, j) = A(i - 1, j) if ci > j
    * A(i, j) = max(A(i - 1, j), vi + A(i - 1, j - ci)) if ci ≤ j


