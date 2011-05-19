fun gcd(a, 0) = a
   | gcd(a, b) = gcd(b, a mod b);


fun map(f, []) = []
  | map(f, x::rest) = f(x) :: map(f, rest);

fun pow(x, 0) = 1
  | pow(x, y) = x * pow(x, y-1);

(*1. *)
fun lcm(x, y) = (x*y) div gcd(x, y);

(*2. *)
fun findRelativelyPrime(x) = 
	let
		fun getRelPrime(x, y) = if gcd(x, y) = 1 then y else getRelPrime(x, y+1);
	in
		getRelPrime(x, 2)
	end;

(*3. *)

fun findModInverse(r, m) = 
	let
		fun ext_gcd(a, b) = if a mod b = 0 then (0, 1) else
        		let
				val ans = ext_gcd(b, a mod b);
				val ret = (#2(ans), (#1(ans)-(#2(ans)*(a div b))));
			in
				ret
			end;
		fun getExtGcd() = 
			let
				val toReturn = #1(ext_gcd(r, m));
			in
				if toReturn < 0 then toReturn + m else toReturn
			end;
	in
		getExtGcd()
	end;


(*4. *)
fun findKeys(p, q) = 
	let
		val n = p*q;
		val m = lcm(p-1, q-1);
		val r = findRelativelyPrime(m);
		val s = findModInverse(r, m);
	in
		(r, n, s)
	end;

(*5. *)
fun cryptChar(c, 0, n) = 1
  | cryptChar(c, r, n) = (c*cryptChar(c, r-1, n)) mod n;


fun encrypt(M, r, n) =
    map(fn x => cryptChar(Char.ord(x), r, n), explode(M));

fun decrypt(M, s, n) = 
    implode(map(fn x => Char.chr(cryptChar(x, s, n)), M));


val keys = findKeys(p, q);
val R = #1(keys);
val N = #2(keys);
val S = #3(keys);
val codedMsg = encrypt("secret message", R, N);
val oldMsg = decrypt(codedMsg, S, N);
