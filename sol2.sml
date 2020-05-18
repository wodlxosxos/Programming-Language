datatype expr = NUM of int
			| PLUS of expr * expr
			| MINUS of expr * expr

datatype formula = TRUE
				| FALSE
				| NOT of formula
				| ANDALSO of formula * formula
				| ORELSE of formula * formula
				| IMPLY of formula * formula
				| LESS of expr * expr

datatype 'a lazyList = nullList
					| cons of 'a * (unit -> 'a lazyList)
fun sub(k) =
	case k of
		  NUM i				=> i
		| PLUS (e1, e2)		=> (sub e1) + (sub e2)
		| MINUS (e1, e2)	=> (sub e1) - (sub e2)

fun eval(e) =
	case e of
		  TRUE				=> true
		| FALSE				=> false
		| NOT e1			=> if (eval e1) = true then false
								else true
		| ANDALSO(e1, e2)	=> (eval e1) andalso (eval e2)
		| ORELSE(e1, e2)	=> (eval e1) orelse (eval e2)
		| IMPLY(e1, e2)		=> if ((eval e1) andalso not(eval e2)) = true then false
								else true
		| LESS(e1, e2)		=> if (sub (NUM(sub e1))) < (sub(NUM(sub e2))) then true
								else false

type name = string

datatype metro =  STATION of name
				| AREA of name * metro
				| CONNECT of metro * metro

fun rmStation(m: metro, mL: metro list) = 
	if null mL
	then []
	else if m = hd(mL)
	then rmStation(m, tl(mL))
	else hd(mL) :: rmStation(m, tl(mL))
	
fun checkMetro(m : metro) = 
	case m of
		  STATION s 		=> false
		| CONNECT(n1, m1)   => (checkMetro n1) andalso (checkMetro m1)
		| AREA(n1, m1)		=> let
									fun areas(aList, n) =
										if null aList
										then false
										else
											case n of
												  STATION s			=> (s = hd(aList)) orelse (areas(tl(aList), n))
												| CONNECT(m20, m21)	=> (areas(aList, m20)) andalso (areas(aList, m21))
												| AREA (n2, m2)     => areas(n2 ::  aList, m2)
									in
										areas([n1], m1)
									end

fun seq(first : int, last : int) =
	if first = last
	then cons(first, fn () => nullList)
	else cons(first, fn () => seq(first+1, last))

fun infSeq(first: int) =
	cons(first, fn () => infSeq(first+1))

fun firstN(lazyL: 'a lazyList, n: int) = 
	if n < 1
	then []
	else
		case lazyL of
			  nullList => []
			| cons(a, f) => [a] @ firstN(f (), n-1)

fun Nth(lazyL: 'a lazyList, n: int) = 
	if n < 1
	then NONE
	else
		case lazyL of
				  nullList => NONE
				| cons(a, f) => if n =1 
								then SOME a
								else Nth(f(), n-1)

fun filterMultiples(lazyL: int lazyList, n: int) = 
	case lazyL of
			  nullList => nullList
			| cons(a,f) => if (a mod n) = 0
						   then filterMultiples(f(), n)
						   else cons(a, fn () => filterMultiples(f(), n))

fun sieve(lazyL: int lazyList) = 
	case lazyL of
			  cons(a, f) => cons(a, fn() => sieve(filterMultiples(f(), a)))
			| nullList => nullList

fun primes() =
	sieve(infSeq(2))
