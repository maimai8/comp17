let rec gcd2 m n =
  let v_13=n in
  let v_14=0 in
  if v_13=v_14
  then m
  else let amari=let v_17=m in
                 let v_18=n in
                 (v_17 mod v_18) in
       let v_15=n in
       let v_16=amari in
       (gcd2 v_15 v_16)
in

let rec gcd m n =
  let v_7=n in
  let v_8=m in
  if v_7<v_8
  then let v_11=m in
       let v_12=n in
       (gcd2 v_11 v_12)
  else let v_9=n in
       let v_10=m in
       (gcd2 v_9 v_10)
in

let v_1=let v_5=12 in
        let v_6=6 in
        (v_5+v_6) in
let v_2=let v_3=3 in
        let v_4=8 in
        (v_3*v_4) in
(gcd v_1 v_2)
