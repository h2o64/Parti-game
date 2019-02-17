(* Float operations *)
let float_precision = 0.00000001;;
let neg_float_precision = -0.00000001;;
let is_0_f x = (abs_float x) < float_precision;;
let is_pos_f x = x > neg_float_precision;;
let is_neg_f x = x < float_precision;;

(* Squared distances *)
let distance_int a b length =
	let ret = ref 0 in
	for i = 0 to (length-1) do
		ret := !ret + ((a.(i) - b.(i)) * (a.(i) - b.(i)))
	done;(sqrt (float_of_int !ret));;
let distance_float a b length =
	let ret = ref 0. in
	for i = 0 to (length-1) do
		ret := !ret +. ((a.(i) -. b.(i)) *. (a.(i) -. b.(i)))
	done;(sqrt !ret);;

(* Operations *)
let add = (+.);;
let minus = (-.);;
let mul = ( *. );;
let div = (/.);;
let abs_f = abs_float;;
let biggest = max_float;;
let lowest = min_float;;
let zero = 0.;;
let one = 1.;;
let m_one = -1.;;
let two = 2.;;
let four = 4.;;
let to_int = int_of_float;;
let of_int = float_of_int;;
let to_float = fun x -> x;;
let of_float = to_float;;
let precision = 0.00000001;;
let neg_precision = -0.00000001;;
let is_0 x = (abs_float x) < float_precision;;
let is_pos x = x > neg_float_precision;;
let is_neg x = x < float_precision;;
let is_pos_strict x = (x > neg_precision) && (x <> 0.);;
let is_pos_strict x = (x < precision) && (x <> 0.);;
let distance_f = distance_float;;