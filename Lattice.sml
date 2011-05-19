(* latt = set of items in the lattice *)
(* po = a relation that takes two items and returns true or false if they are related *)

fun latticeOps(latt, po) = 
	let
		fun isLUB(cand, []) = true
		  | isLUB(cand, ub::rest) = if cand < ub then isLUB(cand, rest) else false;
		fun LUB(a, b) = 
	    	let
				fun mkList([]) = []
				  | mkList(item::rest) = if po(a, item) andalso po(b, item) then item::mkList(rest) else mkList(rest);
				val ubs = mkList(latt);
				fun findLUB([]) = 1
				  | findLUB(i::rest) = if isLUB(i, ubs) then i else findLUB(rest);
			in
				findLUB(ubs)
			end;

		fun isGLB(cand, []) = true
		  | isGLB(cand, lb::rest) = if cand > lb then isGLB(cand, rest) else false;
		fun GLB(a, b) = 
	    	let
				fun mkList([]) = []
				  | mkList(item::rest) = if po(item, a) andalso po(item, b) then item::mkList(rest) else mkList(rest);
				val lbs = mkList(latt);
				fun findGLB([]) = 0
				  | findGLB(i::rest) = if isGLB(i, lbs) then i else findGLB(rest);
			in
				findGLB(lbs)
			end;

	in
		(LUB, GLB)
	end;

val: (*compute list of all upper bounds of a and b*)
fun findXXX = (*given a list of upper bounds, find the least, using helper function*)
