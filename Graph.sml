
val graph = [(1, [2, 5]), (2, [5, 6]), (3, [4]), (4, []), (5, [3, 6]), (6, [4, 7]), (7, [])];



fun getTuple(lbl, (a, b)::rest) = 
if lbl = a
then (a, b)
else getTuple(lbl, rest);


fun getadj(item, []) = []
| getadj(item, (a, b)::rest) = 
if item = a
then b
else getadj(item, rest); 



fun findPath(start, end, []) = []
| findPath(start, end, graph) = 
  if isSome(findSub(getadj(start, graph), end, graph)
  then
  else

start::findSub(getadj(start, graph), end, graph);


fun findSub([], end, graph) = NONE
| findSub(a::rest, end, graph) =
    if a = end
    then SOME [a]
    else 
	 if isSome(findSub(getadj(a, graph), end, graph))
	 then SOME a::valOf(findSub(getadj(a, graph), end, graph))
	 else findSub(rest, end, graph);

fun findSub([], end, graph) = NONE
| findSub(a::rest, end, graph) = 




	let
		
getTuple(start, graph)

buildPath() = 
	    
	    fun buildPath(x) = if x = end then [x] else
	    

	    fun checkList([]) = false
	    | checkList(a::rest) = if a = end then true else checkList(rest);

		
	in
		if checkList(List) then List	
		
	end;
