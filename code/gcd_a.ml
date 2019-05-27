let rec gcd2 _R_1 _R_2 =
  let _R_11=_R_1 in
  let _R_10=_R_2 in
  let _R_9=_R_10 in
  let _R_8=0 in
  if _R_9=_R_8
  then _R_11
  else let _R_7=let _R_6=_R_11 in
                let _R_5=_R_10 in
                (_R_6 mod _R_5) in
       let _R_4=_R_10 in
       let _R_3=_R_7 in
       let _R_1=_R_4 in
       let _R_2=_R_3 in
       (gcd2 _R_1 _R_2)
in

let rec gcd _R_1 _R_2 =
  let _R_11=_R_1 in
  let _R_10=_R_2 in
  let _R_9=_R_10 in
  let _R_8=_R_11 in
  if _R_9<_R_8
  then let _R_7=_R_11 in
       let _R_6=_R_10 in
       let _R_1=_R_7 in
       let _R_2=_R_6 in
       (gcd2 _R_1 _R_2)
  else let _R_5=_R_10 in
       let _R_4=_R_11 in
       let _R_1=_R_5 in
       let _R_2=_R_4 in
       (gcd2 _R_1 _R_2)
in

let _R_11=let _R_10=12 in
          let _R_9=6 in
          (_R_10+_R_9) in
let _R_8=let _R_7=3 in
         let _R_6=8 in
         (_R_7*_R_6) in
let _R_1=_R_11 in
let _R_2=_R_8 in
(gcd _R_1 _R_2)
