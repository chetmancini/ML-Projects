fun firstTrue(true::rest) = 1
  | firstTrue(false::rest) = 1 + firstTrue(rest);

fun falseEveryN(n, count, []) = []
  | falseEveryN(n, 1, x::rest) = false::falseEveryN(n, n, rest)
  | falseEveryN(n, m, x::rest) = x::falseEveryN(n, m-1, rest);

fun split(0, r) = ([], r)
  | split(n, first::rest) =
    let val (a,b) = split(n-1, rest) in(first::a, b) end;

fun oneShake(nums) =
   let
       val factor = firstTrue(nums); 
       val (a, b) = split(factor, nums); 
   in
      a@falseEveryN(factor, factor, b)
end;

fun halfSieve(count, []) = []
  | halfSieve(count, true::rest) = 
    true::halfSieve(count+1, falseEveryN(count, count, rest))
  | halfSieve(count, false::rest) = 
    false::halfSieve(count + 1, rest);

fun makeStream(0) = []
  | makeStream(n) = true::makeStream(n-1);

fun falseFirst(stream) = false::tl(stream);

fun makeNumList(count, []) = []
  | makeNumList(count, true::rest) = count::makeNumList(count + 1, rest)
  | makeNumList(count, false::rest) = makeNumList(count+1, rest);

fun sieve(n) =
  makeNumList(1, halfSieve(1, falseFirst(makeStream(n))));



datatype tree = Leaf of int | Internal of tree list; 

fun isOdd(0) = false
| isOdd(1) = true
| isOdd(n) = isOdd(n-2);

fun isEven(n) = not(isOdd(n));

fun fib(0) = 1
| fib(1) = 1
| fib(n) = fib(n-1) + fib(n-2);

fun map(f, []) = []
  | map(f, x::rest) = f(x) :: map(f, rest);

fun filter(p, []) = []
  | filter(p, x::rest) = if p(x) then x::filter(p, rest) else filter(p, rest);

fun accumulate(f, s, []) = s
  | accumulate(f, s, x::rest) = 
        f(x, accumulate(f, s, rest));

accumulate(fn (x, y) => x * y, 1, [1,2,3,4,5]);

fun enumerateTree(Leaf(x)) = [x]
  | enumerateTree(Internal(a::rest)) = 
           enumerateTree(a)@enumerateTree(rest)
  | enumerateTree(Internal([])) = [];

fun square(x) = x * x;

fun plus(x, y) = x + y

fun sumOddSquares(tr) = 
  accumulate(plus, 0, map(square, filter(isOdd, enumerateTree(tr))));

fun cons(a, b) = a::b;

fun enumerateInterval(low, high) =
  if low > high
  then []
  else low::enumerateInterval(low+1, high);


fun evenFibs(n) =
  accumulate(cons, [], filter(isEven, map(fib, enumerateInterval(1, n))));

