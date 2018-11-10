(* Graphic Library *)
#load "graphics.cma";;

module KDTrees :
  sig
		val getFormat : int -> int -> string
		val drawPoints : int array array -> unit
		val randomPoints : int * int -> int -> int array array
		type 'a tree = EmptyTree | Node of 'a array * 'a tree * 'a tree
		val swap : 'a array -> int -> int -> unit
		val partition_hoare : 'a array array -> int -> int -> int -> int
		val partition_lomuto : 'a array array -> int -> int -> int -> int
		val quicksort : 'a array array -> int -> int -> int -> unit
		val constructKDT : 'a array array -> 'a tree
		val drawTree : int * int -> int tree -> unit
		val drawBorders : int tree -> unit
		val addTree : 'a array -> 'a tree -> int -> 'a tree
		val minimum_t : 'a tree -> int -> int -> int -> 'a array
		val maximum_t : 'a tree -> int -> int -> int -> 'a array
		val removeTree : 'a array -> 'a tree -> int -> 'a tree
		val uniformPoints : int * int -> int -> int -> bool -> int array array
		val distance_int : int array -> int array -> int -> float
		val distance_float : float array -> float array -> int -> float
		type 'a distance_tools = {
			op_to_float : 'a -> float;
			distance_f : 'a array -> 'a array -> int -> float;
		}
		val int_tools : int distance_tools
		val float_tools : float distance_tools
		val nns : 'a tree -> 'a array -> 'a distance_tools -> int -> float * 'a array
		val compare_path : 'a tree -> 'a array -> int -> int -> unit
		val checkTree : 'a tree -> int -> int -> int
		val sanity_test : int -> int array * int array array
  end =

  struct
		(* Get the right window format *)
		let getFormat height width =
			let s_height = string_of_int height in
			let s_width = string_of_int width in
			String.concat "" [" ";s_height;"x";s_width];;

		(* Draw a set of 2D points *)
		let drawPoints points =
			(* Sorts points *)
			let n = Array.length points in
			Array.fast_sort compare points;
			(* Make the window - Force (0,0) to be the origin *)
			let (max_x,max_y) = (points.(n-1).(0),points.(n-1).(1)) in
			Graphics.open_graph (getFormat max_x max_y);
			(* Fill the window *)
			for i = 0 to (n-1) do
				Graphics.plot points.(i).(0) points.(i).(1);
			done;;

		(* Get a random 2D points set *)
		let randomPoints (h,w) n =
			let ret = Array.make (n+1) [|0;0|] in
			for i = 0 to n do
				ret.(i) <- [|(Random.int w);(Random.int h)|];
			done;ret;;

		(* Constuct a tree *)
		let example = [| [|2;3|]; [|5;4|]; [|9;6|]; [|4;7|]; [|8;1|]; [|7;2|] |];;
		type 'a tree =
			| EmptyTree
			| Node of ('a array) * ('a tree) * ('a tree);;
		(* Swap in an array *)
		let swap arr i j =
			let tmp = arr.(i) in
			arr.(i) <- arr.(j);
			arr.(j) <- tmp;;
		(* Quicksort *)
 		(* Hoare Partition Scheme *)
		let partition_hoare arr left right depth =
			let pivot = arr.(left).(depth) in
			let i = ref (left - 1) in
			let j = ref (right + 1) in
			try
				while true do
					i := !i + 1;
						while (arr.(!i).(depth) < pivot) do
							j := !j - 1;
							while (arr.(!j).(depth) > pivot) do
								if (!i >= !j) then raise Exit;
								swap arr !i !j
							done;
						done;
				done;(-1);
			with Exit -> !j;;
 		(* Lomuto Partition Scheme *)
		let partition_lomuto arr left right depth =
			let pivot = arr.(right).(depth) in
			let i = ref left in
			for j = left to (right-1) do
				if arr.(j).(depth) < pivot then
					(if !i <> j then swap arr !i j;
					i := !i + 1;)
			done;
			swap arr !i right;
			!i;;
 		(* Sorting algorithm *)
		let rec quicksort arr left right depth =
			if (left < right) then
				(let p = partition_lomuto arr left right depth in
				quicksort arr left (p-1) depth;
				quicksort arr (p+1) right depth);;
		(* Actual construction of the tree *)
		let constructKDT points =
			let dim = Array.length points.(0) in
			let length = Array.length points in
			let rec constructKDT_aux a b depth =
				let new_depth = ((depth+1) mod dim) in
				match abs (a-b) with
					| 0 -> Node(points.(a),EmptyTree,EmptyTree)
					| 1 ->
						(if points.(a).(depth) <= points.(b).(depth) then
							Node(points.(a),EmptyTree,Node(points.(b),EmptyTree,EmptyTree))
						else
							Node(points.(a),Node(points.(b),EmptyTree,EmptyTree),EmptyTree))
					| _ -> (* Sort our portion of points *)
						(let median = (a + b + 1) / 2 in
						quicksort points a b depth;
						Node(points.(median),
								 constructKDT_aux a (median-1) new_depth,
								 constructKDT_aux (median+1) b new_depth)) in
			constructKDT_aux 0 (length-1) 0;;

		(* Print 2D points in a 2D tree *)
		let drawTree (h,w) t =
			(* Make the window  *)
			Graphics.open_graph (getFormat h w);
			let rec drawTree_aux t =
				match t with
					| EmptyTree -> ()
					| Node(point,left,right) -> ((Graphics.plot point.(0) point.(1));
															 drawTree_aux left;
															 drawTree_aux right) in
			drawTree_aux t;;

		(* Draw borders for a 2D Tree *)
		let drawBorders t =
			(* Set various parameters *)
			let (h,w) = (Graphics.size_y (),Graphics.size_x ()) in
			Graphics.set_line_width 1;
			(* Count node parity for X/Y *)
			let rec drawBorders_aux tr i median_point side = match tr with
				| EmptyTree -> ()
				| Node(point,left,right) ->
					if ((i mod 2) = 0) then
						(Graphics.set_color Graphics.red;
						if side = 0 then
							(Graphics.moveto point.(0) 0;
							Graphics.lineto point.(0) median_point.(1))
						else
							(Graphics.moveto point.(0) h;
							Graphics.lineto point.(0) median_point.(1)))
					else
						(Graphics.set_color Graphics.blue;
						if side = 0 then
							(Graphics.moveto 0 point.(1);
							Graphics.lineto median_point.(0) point.(1))
						else
							(Graphics.moveto median_point.(0) point.(1);
							Graphics.lineto w point.(1)););
					drawBorders_aux left (i+1) point 0;
					drawBorders_aux right (i+1) point 1 in
			drawBorders_aux t 0 [|h;w|] 0;;

		(* Add an element to tree - Breaks balancing *)
		let addTree x t dim =
			(* Need to keep the depth in mind *)
			let rec addTree_aux tr depth =  match tr with
				| EmptyTree -> Node(x,EmptyTree,EmptyTree)
				| Node(point,left,right) ->
					if (point = x) then tr
					else
						(if (x.(depth) < point.(depth)) then
							Node(point,(addTree_aux left ((depth+1) mod dim)),right)
						else
							Node(point,left,(addTree_aux right ((depth+1) mod dim)));); in
			addTree_aux t 0;;

		(* Find the max/min for the targeted depth *)
		let minimum_t t init_depth depth dim =
			let rec minimum_t_aux cur_t cur_depth = match cur_t with
				| EmptyTree -> failwith "minimum_t_aux: Error empty tree"
				| Node(x,left,right) ->
					let new_depth = ((cur_depth+1) mod dim) in
					(* Tree splits on the dimension we’re searching
						 => only visit left subtree *)
					if (cur_depth = depth) then
						if (left = EmptyTree) then (x.(depth),x)
						else minimum_t_aux left new_depth
					else
						(* Tree splits on a different dimension
							 => have to search both subtrees
							 Avoid empty trees *)
						if (left = EmptyTree) && (right = EmptyTree) then (x.(depth),x)
						else if (left = EmptyTree) then min (minimum_t_aux right new_depth) (x.(depth),x)
						else if (right = EmptyTree) then min (minimum_t_aux left new_depth) (x.(depth),x)
						else min (min (minimum_t_aux right new_depth) (minimum_t_aux left new_depth))
										 (x.(depth),x) in
			let (_,ret) = minimum_t_aux t init_depth in
			ret;;

		let maximum_t t init_depth depth dim =
			let rec maximum_t_aux cur_t cur_depth = match cur_t with
				| EmptyTree -> failwith "maximum_t_aux: Error empty tree"
				| Node(x,left,right) ->
					let new_depth = ((cur_depth+1) mod dim) in
					(* Tree splits on the dimension we’re searching
						 => only visit right subtree *)
					if (cur_depth = depth) then
						if (right = EmptyTree) then (x.(depth),x)
						else maximum_t_aux right new_depth
					else
						(* T splits on a different dimension
							 => have to search both subtrees
							 Avoid empty trees *)
						if (left = EmptyTree) && (right = EmptyTree) then (x.(depth),x)
						else if (left = EmptyTree) then max (maximum_t_aux right new_depth) (x.(depth),x)
						else if (right = EmptyTree) then max (maximum_t_aux left new_depth) (x.(depth),x)
						else max (max (maximum_t_aux right new_depth) (maximum_t_aux left new_depth))
										 (x.(depth),x) in
			let (_,ret) = maximum_t_aux t init_depth in
			ret;;

		(* Better removal in kd-tree *)
		let removeTree target t dim =
			let rec removeTree_aux cur_target cur_t depth = match cur_t with
				| EmptyTree -> failwith "removeTree_aux: Point can't be found"
				| Node(x,left,right) ->
					let new_depth = ((depth+1) mod dim) in
					(* Point is found *)
					if (cur_target = x) then
						(* Use the minimum of the right depth from right tree *)
						(if (right <> EmptyTree) then
							(*  Swap subtrees and use min(cd) from new right *)
							(let rep = (minimum_t right new_depth depth dim) in
							Node(rep,left,(removeTree_aux rep right new_depth)))
						else if (left <> EmptyTree) then
							(let rep = (maximum_t left new_depth depth dim) in
							Node(rep,(removeTree_aux rep left new_depth),right))
						else
							(* Just a leaf *)
							(EmptyTree);)
					else if (cur_target.(depth) < x.(depth)) then
							(Node(x,(removeTree_aux cur_target left new_depth),right))
					else
							(Node(x,left,(removeTree_aux cur_target right new_depth))); in
			removeTree_aux target t 0;;

		(* Get a random 2D points set *)
		let uniformPoints (h,w) per_line per_row random =
			let ret = Array.make (((per_line+1)*(per_row+1))+1) [|0;0|] in
			let delta_x = w/per_line in
			let delta_y = h/per_row in
			let cur = ref 0 in
			for i = 0 to per_line do
				for j = 0 to per_row do
					ret.(!cur) <- [|i*delta_x;j*delta_y|];
					cur := !cur + 1;
				done;
			done;
			(* Random-izer 3000 *)
			if random then
				(for i = 0 to (per_line*per_row)/3 do
					swap ret (Random.int (per_line*per_line)) (Random.int (per_line*per_line));
				done;ret)
			else ret;;

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

		(* Algebra toolpack *)
		type 'a distance_tools = {
				op_to_float : ('a -> float);
				distance_f : ('a array -> 'a array -> int -> float);
				};;
		let int_tools = {
				op_to_float = float_of_int;
				distance_f = distance_int;
				};;
		let float_tools = {
				op_to_float = (let f x = x in f);
				distance_f = distance_float;
				};;

		(* Nearest Neighbor Search *)
		let nns t target toolpack dim =
			(* Algorithm data *)
			let w = ref max_float in
			let p = ref target in
			let rec nns_aux cur_t depth = match cur_t with
				| EmptyTree -> ()
				| Node(x,left,right) ->
					let new_depth = ((depth+1) mod dim) in
					(* Check is the point is better than the best *)
					let new_w = toolpack.distance_f target x dim in
					if (new_w < !w) && (x <> target) then
						(w := new_w;
						p := x);
					(* Visit subtrees *)
					let target_depth = toolpack.op_to_float target.(depth) in
					let x_depth = toolpack.op_to_float x.(depth) in
					if target.(depth) <= x.(depth) then
						(nns_aux left new_depth;
						if (target_depth +. !w) >= x_depth then nns_aux right new_depth)
					else
						(nns_aux right new_depth;
						if (target_depth -. !w) <= x_depth then nns_aux left new_depth) in

			nns_aux t 0;
			(!w,!p);;

		(* Path to a node *)
		let rec compare_path t a depth dim = match t with
			| EmptyTree -> ()
			| Node(x,left,right) ->
				if x = a then print_string " ok\n"
				else
					if a.(depth) <= x.(depth) then
						(print_string "0";
						compare_path left a ((depth+1) mod dim) dim)
					else
						(print_string "1";
						compare_path right a ((depth+1) mod dim) dim);;
					
		(* Check if it's a kd-tree *)
		let rec checkTree t depth dim = match t with
			| EmptyTree -> 0
			| Node(x,EmptyTree,EmptyTree) -> 0
			| Node(a,left,right) ->
				let new_depth = ((depth+1) mod dim) in
				match (left,right) with
					| EmptyTree,EmptyTree -> 0 
					| Node(x,_,_),EmptyTree ->
						if (x.(depth) > a.(depth)) then 1 + (checkTree left new_depth dim)
						else (checkTree left new_depth dim)
					| EmptyTree,Node(y,_,_) ->
						if (y.(depth) < a.(depth)) then 1 + (checkTree right new_depth dim)
						else (checkTree right new_depth dim)
					| (Node(x,_,_),Node(y,_,_)) ->
						let tmp = ref 0 in
						if (x.(depth) > a.(depth)) then tmp := !tmp + 1;
						if (y.(depth) < a.(depth)) then tmp := !tmp + 1;
						!tmp + (checkTree left new_depth dim) + (checkTree right new_depth dim);;

	(* Sanity test for NNS *)
	let sanity_test n =
		let array_test = (randomPoints (800,800) n) in
		let tree_test = constructKDT array_test in
		Graphics.set_color Graphics.red;
		drawTree (800,800) tree_test;
		(* Choose a point *)
		let num = Random.int n in
		Graphics.set_color Graphics.blue;
		Graphics.draw_circle array_test.(num).(0) array_test.(num).(1) 10;
		(* Find the closest *)
		let (_,closest) = nns tree_test array_test.(num) int_tools 2 in
		Graphics.set_color Graphics.green;
		Graphics.draw_circle closest.(0) closest.(1) 10;
		(* Return the interssting stuff *)
		(array_test.(num),array_test);;

	end
