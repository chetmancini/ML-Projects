datatype huffTree = Leaf of string * int  
                  | Internal of huffTree * huffTree * string * int;

fun weight(Leaf(sym, w)) = w
  | weight(Internal(left, right, sym, w) = w;

fun symbols(Leaf(sym, w)) = sym
  | symbols(Internal(left, right, sym, w)) = 
          symbols(left) ^ symbols(right);

fun makeTree(left, right) = 
     Internal(left, right, symbols(left) ^ symbols(right),
              weight(left) + weight(right));

datatype bit = One | Zero;

val sampleTree = makeTree(Leaf("A", 4),
                 makeTree(Leaf("B", 2),
                 makeTree(Leaf("D", 1), Leaf("C", 1))));
      
val sampleMessage = [Zero, One, One, Zero, Zero, One, Zero,
                     One, Zero, One, One, One, Zero];


fun chooseBranch(Zero, Internal(left, right, s, w)) = left
  | chooseBranch(One, Internal(left, right, s, w)) = right;

fun decode(bits, tree) =
   let
      fun decode1([], currentBranch) = []
        | decode1(b::rest, currentBranch) =  ??
           let val nextBranch =  in
           case nextBranch of
             Leaf(sym, w) =>  ??
           | Internal(left, right, syms, w) => ??
           end;
   in
     decode1(bits, tree)
   end;


fun encodeSymbol(sym, tree) = 
   let 
      fun encodeSymbol1(sy, Leaf(st, w)) = 
                       ??
        | encodeSymbol1(sy, Internal(left, right, st, w)) =
             case (encodeSymbol1(sy, left), encodeSymbol1(sy, right)) of
                (NONE, NONE) => ??
              | (NONE, SOME bits) => ??
              | (SOME bits, x) => ??;
    in
      valOf(encodeSymbol1(sym,tree))
    end;

fun encode([], tree) = []
  | encode(fst::rest, tree) = 
       encodeSymbol(fst, tree)@encode(rest, tree);
