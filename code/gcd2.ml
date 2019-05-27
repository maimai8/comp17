(* find greatest common divisor of 18 and 24 *)

let rec gcd2 m n =
  if n = 0 then m
  else let amari = m mod n
       in gcd2 n amari
in

let rec gcd m n =
  if m <= n then gcd2 n m
	    else gcd2 m n
in

let rec loop1 n =
  let result = gcd 18 24 in
  if n = 0 then result
	   else loop1 (n - 1)
in

let rec loop2 n =
  let result = loop1 20000 in
  if n = 0 then result
	   else loop2 (n - 1)
in

loop2 20000
