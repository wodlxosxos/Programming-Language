fun merge (fl: int list, sl: int list) =
	if null fl
	then sl
	else
	if hd(fl) > hd(sl)
		then hd(sl) :: merge(fl, tl(sl))
		else hd(fl) :: merge(tl(fl), sl)

fun reverse (xs: int list) =
	let
		fun sum(arr1: int list, arr2: int list) = 
			if null arr1
			then arr2
			else
				sum(tl(arr1), (hd(arr1) :: arr2))
	in
		sum(xs, [])
	end

fun sigma (a: int, b: int, f: int->int) =
	if a = b
	then f(a)
	else
		f(a) + sigma(a+1, b, f)

fun digits (dg: int) =
	let
		fun comp(a:int, b:int) =
			if (b div a) > 9
			then comp(10*a, b)
			else
			if( a > 1 )
			then (b div a) :: comp((a div 10), (b - (b div a)*a))
			else (b div a) :: []
	in
		comp(1, dg)
	end

fun additivePersistence (n: int) =
	let
		fun sum(a: int list) = 
			if null (tl a)
			then hd(a)
			else (hd(a) + sum(tl(a)))
		val arr = digits(n)
		val plus = 1
	in
		if (sum(arr) div 10) = 0
		then plus
		else plus + additivePersistence(sum(arr))
	end

fun digitalRoot (n: int) =
	let
		fun sum(a: int list) =
			if null (tl a)
			then hd(a)
			else (hd(a) + sum(tl(a)))
		val arr = digits(n)
	in
		if (sum(arr) div 10) = 0
		then sum(arr)
		else digitalRoot(sum(arr))
	end
