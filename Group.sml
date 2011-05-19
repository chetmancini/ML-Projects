fun getGenerators(n) = 
	let
		fun gcd(a, 0) = a
		| gcd(a, b) = gcd(b, a mod b);

		fun filter(p, []) = []
		  | filter(p, x::rest) = if p(x) then x::filter(p, rest) else filter(p, rest);

		fun reverse([]) = []
		  | reverse(x::rest) = reverse(rest)@[x];

		fun count([]) = 0
		  | count(x::rest) = 1 + count(rest);

		fun getIntegers(0) = []
		  | getIntegers(x) = x::getIntegers(x-1);

		fun gcdIsOne(x) = gcd(x, n) = 1;

		fun getElements(x) = reverse(filter(gcdIsOne, getIntegers(x)));
		
		fun getM(x) = count(getElements(x));

		fun isGenerator(a) = 
			let
				fun apply(x, y) = (x * y) mod n;
				val applications = ref 0;
				val current = ref 1;
				fun rApply(0) = !current
				  | rApply(x) = (current := apply(a, !current); 
						applications := !applications + 1; 
						rApply(x-1));
				val m = getM(n);
			in
				(rApply(m);
				if !applications = m andalso a <> 1 then true else false)
			end;

		fun pow(x, 0) = 1
		  | pow(x, y) = (x * pow(x, y-1)) mod n;

		fun getFirstGenerator([]) = NONE 
		  | getFirstGenerator(x::rest) = if isGenerator(x) then SOME x else getFirstGenerator(rest)

		fun getAllGenerators(a, []) = []
		  | getAllGenerators(a, primeWithM::rest) = pow(a, primeWithM)::getAllGenerators(a, rest);

	in
		getAllGenerators(valOf(getFirstGenerator(getElements(n))), getElements(getM(n)))
	end;
