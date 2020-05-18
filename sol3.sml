datatype pattern = Wildcard | Variable of string | UnitP
				  | ConstP of int | TupleP of pattern list
				  | ConstructorP of string * pattern

datatype valu = Const of int | Unit | Tuple of valu list
			  | Constructor of string * valu

fun check_pat(p) = 
	let
		fun dist(li) = 
			let
				fun checkVal(v: string) = 
					List.exists(fn chk => v = chk)
			in
				if null (tl li) 
				then not(checkVal(hd li) (tl li))
				else not(checkVal(hd li) (tl li)) andalso dist(tl li)
			end
		fun getList(pat) = 
			case pat of
				  Variable s 			=> [s]
				| TupleP tup			=> let 
												fun sumList(t) = 
													if null (tl t)
													then getList(hd t)
													else getList(hd t) @ sumList(tl t)
											in
												sumList(tup)
											end
				| ConstructorP (s, pt)	=> getList(pt)
				| _ 	=> []
	in
		dist(getList(p))
	end

fun match(v, p) = 
		case (v, p) of
			  (_, Wildcard) 							=> SOME []
			| (v, Variable s) 							=> SOME [(s,v)]
			| (Unit, UnitP)								=> SOME []
			| (Const v1,ConstP s1)						=>  if v1 = s1
															then SOME []
															else NONE
			| (Tuple vs,TupleP ps)					=>  let 
															fun tupleMatch(vf, pf) = 
																case (vf, pf) of
																	  ([], [])					=> SOME []
																	| (vt::taleV, pt::taleP)	=> 	if match(vt,pt) = NONE orelse tupleMatch(taleV, taleP) = NONE
																									then NONE
																									else SOME (valOf(match(vt,pt)) @ valOf(tupleMatch(taleV, taleP)))
														in
															if List.length(vs) = List.length(ps)
															then tupleMatch(vs, ps)
															else NONE
														end
			| (Constructor(s2, v1) , ConstructorP(s1, p1))	=> 	if s2 = s1
																then match(v1, p1)
																else NONE
			| (_,_)					=> NONE
	
type name = string
datatype RSP = 
	  ROCK
	| SCISSORS
	| PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament = 
	  PLAYER of name * (RSP strategy ref)
	| MATCH of tournament * tournament

fun onlyOne(one:RSP) = Cons(one, fn () => onlyOne(one))
fun alterTwo(one:RSP, two:RSP) = Cons(one, fn () => alterTwo(two, one))
fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn () => alterThree(two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)

fun next(strategyRef) = 
	let
		val Cons(rsp, func) = !strategyRef
	in
		strategyRef := func();
		rsp
	end

fun winner(r1, r2) =
	case(r1, r2) of
		  (ROCK, SCISSORS)	=> true
		| (SCISSORS, PAPER)	=> true
		| (PAPER, ROCK)		=> true
		| (_,_)				=> false

fun whosWinner(t) =
		let
			fun rspGame(p1, p2) = 
				case(p1, p2) of
					  (MATCH(t1, t2), MATCH(t3, t4))	=> rspGame(rspGame(t1, t2), rspGame(t3, t4))
					| (MATCH(t1, t2), PLAYER(n1, s1))	=> rspGame(rspGame(t1, t2), PLAYER(n1, s1))
					| (PLAYER(n1, s1), MATCH(t1, t2))	=> rspGame(PLAYER(n1, s1), rspGame(t1, t2))
					| (PLAYER(n1, s1), PLAYER(n2, s2))	=>  let
																val nxt1 = next(s1)
																val nxt2 = next(s2)
															in
																if nxt1 = nxt2
																then rspGame(PLAYER(n1, s1), PLAYER(n2, s2))
																else if winner(nxt1, nxt2)
																then PLAYER(n1, s1)
																else PLAYER(n2, s2)
															end
		in
			case t of
				  MATCH(t1, t2)		=> rspGame(t1, t2)
				| PLAYER(t1, t2)	=> t
		end
