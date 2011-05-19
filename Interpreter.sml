datatype state = A | B | C | D | Acc | Rej;

fun m1(#"0", A) = B
  | m1(#"1", A) = A
  | m1(#"0", B) = B
  | m1(#"1", B) = C
  | m1(#"0", C) = B
  | m1(#"1", C) = D
  | m1(#"0", D) = Acc
  | m1(#"1", D) = A
  | m1(#"0", Acc) = Acc
  | m1(#"1", Acc) = Acc
  | m1(_,_) = Rej;

fun run(x) = check2(m1, A, [Acc], x);


fun accumulate(f, s, []) = s
  | accumulate(f, s, x::rest) = 
        f(x, accumulate(f, s, rest));

fun contains(x, []) = false
  | contains(x, y::rest) = if x = y then true else contains(x, rest);

fun accumulate2(f, s, []) = s
  | accumulate2(f, s, x::rest) = 
    accumulate2(f, f(x, s), rest);

fun check(machine, initialState, acceptStates, str) =
    contains(accumulate2(machine, initialState, explode(str)),
             acceptStates);

datatype state = A | B | C | Acc | Rej;

datatype stkSymb = S | T;

fun m2(#"0", (A, stk)) = (B, S::stk)
  | m2(#"0", (B, stk)) = (B, T::stk)
  | m2(#"1", (B, T::stk)) = (C, stk)
  | m2(#"1", (B, S::stk)) = (Acc, [])
  | m2(#"1", (C, T::stk)) = (C, stk)
  | m2(#"1", (C, S::stk)) = (Acc, [])
  | m2(_, _) = (Rej, []);

fun check2(machine, initialState, acceptStates, str) =
    let val (fin, _) = accumulate2(machine, initialState, explode(str));
    in contains(fin, acceptStates) end;

datatype stkSymb = Paren | Curly | Square | ParenBot | CurlyBot | SquareBot;

datatype state = Balance | Wait | Rej;

fun m3(#"(", (Balance, stk)) = (Wait, ParenBot::stk)
  | m3(#"{", (Balance, stk)) = (Wait, CurlyBot::stk)
  | m3(#"[", (Balance, stk)) = (Wait, SquareBot::stk)
  | m3(#"(", (Wait, stk)) = (Wait, Paren::stk)
  | m3(#"{", (Wait, stk)) = (Wait, Curly::stk)
  | m3(#"[", (Wait, stk)) = (Wait, Square::stk)
  | m3(#")", (Wait, Paren::stk)) = (Wait, stk)
  | m3(#"}", (Wait, Curly::stk)) = (Wait, stk)
  | m3(#"]", (Wait, Square::stk)) = (Wait, stk)
  | m3(#")", (Wait, ParenBot::stk)) = (Balance, stk)
  | m3(#"}", (Wait, CurlyBot::stk)) = (Balance, stk)
  | m3(#"]", (Wait, SquareBot::stk)) = (Balance, stk)
  | m3(_,_) = (Rej, []);

datatype stkSymb = Paren | BottomParen | Plus;
datatype state = Initial | A | B | C | D | E | Acc | Rej;

fun m4(#"1", (Initial, stk)) = (Acc, stk)
  | m4(#"(", (Initial, stk)) = (A, BottomParen::stk)
  | m4(#"1", (A, stk)) = (B, stk)
  | m4(#"(", (A, stk)) = (A, Paren::stk)
  | m4(#"+", (B, stk)) = (C, stk)
  | m4(#"1", (C, stk)) = (D, stk)
  | m4(#"(", (C, stk)) = (A, Paren::stk)
  | m4(#")", (D, BottomParen::stk)) = (Acc, stk)
  | m4(#")", (D, Paren::stk)) = (E, stk)
  | m4(#")", (E, BottomParen::stk)) = (Acc, stk)
  | m4(#"+", (E, stk)) = (C, stk)
  | m4(#")", (E, Paren::stk)) = (E, stk)
  | m4(_,_) = (Rej, []);

fun m4(#"1", (Initial, stk)) = (Acc, stk)
  | m4(#"(", (Initial, stk)) = (A, BottomParen::stk)
  | m4(#"1", (A, stk)) = (B, stk)
  | m4(#"(", (A, stk)) = (A, Paren::stk)
  | m4(#"+", (B, stk)) = (C, Plus::stk)
  | m4(#"1", (C, stk)) = (D, stk)
  | m4(#"(", (C, stk)) = (A, Paren::stk)
  | m4(#")", (D, Paren::BottomParen::stk)) = (Acc, stk)
  | m4(#")", (D, Plus::BottomParen::stk)) = (Acc, stk)
  | m4(#")", (D, a::[])) = (Rej, [])
  | m4(#")", (D, a::b::[])) = (Rej, [])
  | m4(#")", (D, a::b::stk)) = (E, stk)
  | m4(#")", (E, Paren::BottomParen::stk)) = (Acc, stk)
  | m4(#")", (E, Plus::BottomParen::stk)) = (Acc, stk)
  | m4(#"+", (E, stk)) = (C, Plus::stk)
  | m4(#")", (E, a::[])) = (Rej, [])
  | m4(#")", (E, a::b::[])) = (Rej, [])
  | m4(#")", (E, a::b::stk)) = (E, stk)
  | m4(_,_) = (Rej, []);

fun run(x) = check2(m4, (Initial, []), [Acc], x);
(*These should return true*)
run("1");
run("(1+1)");
run("((1+1)+1)");
run("(1+(1+1))");
run("(1+((1+1)+1))");
(*These should return false*)
run("((1+1))");
run("(1+((1+1)))");
run("(1+(((1+1))+1))");
run("(+)");
run("()");

