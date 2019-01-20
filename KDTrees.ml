(* Graphic Library *)
#load "graphics.cma";;

module KDTrees :
  sig
		val drawPoints : int array array -> unit
		val randomPoints : int * int -> int -> int array array
		type 'a tree = EmptyTree | Node of 'a array * int * 'a tree * 'a tree
		val constructKDT : 'a array array -> ('a tree * 'a array array)
		val drawTree : int tree -> unit
		val drawBorders : int tree -> unit
		val addTree : 'a array -> 'a tree -> int -> int -> 'a tree
		val removeTree : 'a array -> 'a tree -> int -> 'a tree
		val uniformPoints : int * int -> int -> int -> bool -> int array array
		val distance_int : int array -> int array -> int -> float
		val distance_float : float array -> float array -> int -> float
		type 'a distance_tools
		val int_tools : int distance_tools
		val float_tools : float distance_tools
		val nns : 'a tree -> 'a array -> 'a distance_tools -> int -> float * ('a array * int)
		val knns : 'a tree -> 'a array -> int -> 'a distance_tools -> int -> (float * ('a array * int)) array
		val rebalance : 'a tree -> 'a tree
		val intersect_segment : int array -> int array -> int -> int tree -> bool
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
			| Node of ('a array) * int * ('a tree) * ('a tree);;
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
			(* Count each node *)
			let cur_count = ref (-1) in
			let ret_num = Array.make length [||] in
			let get_nb point () =
				cur_count := !cur_count + 1;
				ret_num.(!cur_count) <- point;
				!cur_count in 
			let rec constructKDT_aux a b depth =
				let new_depth = ((depth+1) mod dim) in
				match abs (a-b) with
					| 0 -> Node(points.(a),(get_nb points.(a) ()),EmptyTree,EmptyTree)
					| 1 ->
						(if points.(a).(depth) <= points.(b).(depth) then
							Node(points.(a),(get_nb points.(a) ()),EmptyTree,Node(points.(b),(get_nb points.(b) ()),EmptyTree,EmptyTree))
						else
							Node(points.(a),(get_nb points.(a) ()),Node(points.(b),(get_nb points.(b) ()),EmptyTree,EmptyTree),EmptyTree))
					| _ -> (* Sort our portion of points *)
						(let median = (a + b + 1) / 2 in
						quicksort points a b depth;
						Node(points.(median),(get_nb points.(median) ()),
								 constructKDT_aux a (median-1) new_depth,
								 constructKDT_aux (median+1) b new_depth)) in
			((constructKDT_aux 0 (length-1) 0),ret_num);;

		(* Print 2D points in a 2D tree *)
		let rec drawTree t =
			match t with
				| EmptyTree -> ()
				| Node(point,_,left,right) -> ((Graphics.plot point.(0) point.(1));
														 drawTree left;
														 drawTree right);;

		(* Draw borders for a 2D Tree *)
		let drawBorders t =
			(* Set various parameters *)
			let (h,w) = (Graphics.size_y (),Graphics.size_x ()) in
			Graphics.set_line_width 1;
			(* Count node parity for X/Y *)
			let rec drawBorders_aux tr i median_point side = match tr with
				| EmptyTree -> ()
				| Node(point,_,left,right) ->
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
		let addTree x t count dim =
			(* Need to keep the depth in mind *)
			let rec addTree_aux tr depth =  match tr with
				| EmptyTree -> Node(x,(count+1),EmptyTree,EmptyTree)
				| Node(point,_,left,right) ->
					if (point = x) then tr
					else
						(if (x.(depth) < point.(depth)) then
							Node(point,(count+1),(addTree_aux left ((depth+1) mod dim)),right)
						else
							Node(point,(count+1),left,(addTree_aux right ((depth+1) mod dim)));); in
			addTree_aux t 0;;

		(* Find the max/min for the targeted depth *)
		let minimum_t t init_depth depth dim =
			let rec minimum_t_aux cur_t cur_depth = match cur_t with
				| EmptyTree -> failwith "minimum_t_aux: Error empty tree"
				| Node(x,count,left,right) ->
					let new_depth = ((cur_depth+1) mod dim) in
					(* Tree splits on the dimension we’re searching
						 => only visit left subtree *)
					if (cur_depth = depth) then
						if (left = EmptyTree) then (x.(depth),x,count)
						else minimum_t_aux left new_depth
					else
						(* Tree splits on a different dimension
							 => have to search both subtrees
							 Avoid empty trees *)
						if (left = EmptyTree) && (right = EmptyTree) then (x.(depth),x,count)
						else if (left = EmptyTree) then min (minimum_t_aux right new_depth) (x.(depth),x,count)
						else if (right = EmptyTree) then min (minimum_t_aux left new_depth) (x.(depth),x,count)
						else min (min (minimum_t_aux right new_depth) (minimum_t_aux left new_depth))
										 (x.(depth),x,count) in
			let (_,ret,count) = minimum_t_aux t init_depth in
			(ret,count);;

		let maximum_t t init_depth depth dim =
			let rec maximum_t_aux cur_t cur_depth = match cur_t with
				| EmptyTree -> failwith "maximum_t_aux: Error empty tree"
				| Node(x,count,left,right) ->
					let new_depth = ((cur_depth+1) mod dim) in
					(* Tree splits on the dimension we’re searching
						 => only visit right subtree *)
					if (cur_depth = depth) then
						if (right = EmptyTree) then (x.(depth),x,count)
						else maximum_t_aux right new_depth
					else
						(* T splits on a different dimension
							 => have to search both subtrees
							 Avoid empty trees *)
						if (left = EmptyTree) && (right = EmptyTree) then (x.(depth),x,count)
						else if (left = EmptyTree) then max (maximum_t_aux right new_depth) (x.(depth),x,count)
						else if (right = EmptyTree) then max (maximum_t_aux left new_depth) (x.(depth),x,count)
						else max (max (maximum_t_aux right new_depth) (maximum_t_aux left new_depth))
										 (x.(depth),x,count) in
			let (_,ret,count) = maximum_t_aux t init_depth in
			(ret,count);;

		(* Better removal in kd-tree *)
		let removeTree target t dim =
			let rec removeTree_aux cur_target cur_t depth = match cur_t with
				| EmptyTree -> failwith "removeTree_aux: Point can't be found"
				| Node(x,count,left,right) ->
					let new_depth = ((depth+1) mod dim) in
					(* Point is found *)
					if (cur_target = x) then
						(* Use the minimum of the right depth from right tree *)
						(if (right <> EmptyTree) then
							(*  Swap subtrees and use min(cd) from new right *)
							(let (rep,c) = (minimum_t right new_depth depth dim) in
							Node(rep,c,left,(removeTree_aux rep right new_depth)))
						else if (left <> EmptyTree) then
							(let (rep,c) = (maximum_t left new_depth depth dim) in
							Node(rep,c,(removeTree_aux rep left new_depth),right))
						else
							(* Just a leaf *)
							(EmptyTree);)
					else if (cur_target.(depth) < x.(depth)) then
							(Node(x,count,(removeTree_aux cur_target left new_depth),right))
					else
							(Node(x,count,left,(removeTree_aux cur_target right new_depth))); in
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
			let p = ref (target,(-1)) in
			let target_c = ref (-1) in
			let rec nns_aux cur_t depth = match cur_t with
				| EmptyTree -> ()
				| Node(x,c,left,right) ->
					let new_depth = ((depth+1) mod dim) in
					(* Check is the point is better than the best *)
					let new_w = toolpack.distance_f target x dim in
					(* Remenber the target count just in case *)
					if (x = target) then
						target_c := c
					else
						(if (new_w < !w) then
							(w := new_w;
							p := (x,c)));
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
			if (!p = (target,(-1))) then (0.,(target,!target_c)) else (!w,!p);;

		(* k-Nearest Neighbor Search - TODO: Use a stack *)
		let knns t target k toolpack dim =
			(* Algorithm data *)
			let w = ref max_float in
			let ret = ref [] in
			let target_c = ref (-1) in
			let rec nns_aux cur_t depth = match cur_t with
				| EmptyTree -> ()
				| Node(x,c,left,right) ->
					let new_depth = ((depth+1) mod dim) in
					(* Check is the point is better than the best *)
					let new_w = toolpack.distance_f target x dim in
					(* Remenber the target count just in case *)
					if (x = target) then
						target_c := c
					else
						(* Add it to the list *)
						(ret := (new_w,(x,c))::!ret;
						if (new_w < !w) then w := new_w);
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
			(* Return the best k *)
			let ret_a = (Array.of_list !ret) in
			Array.fast_sort compare ret_a;
			let goal_length = if (Array.length ret_a) > k then k else (Array.length ret_a) in
			let ret = Array.sub ret_a 0 goal_length in
			if ret = [||] then [|(0.,(target,!target_c))|]
			else ret;;

		(* Path to a node *)
		let rec compare_path t a depth dim cur = match t with
			| EmptyTree -> ()
			| Node(x,_,left,right) ->
				if not (x <> a) then print_string cur
				else
					if a.(depth) <= x.(depth) then
						(compare_path left a ((depth+1) mod dim) dim (cur ^ "0");
						compare_path right a ((depth+1) mod dim) dim (cur ^ "1"))
					else
						(compare_path right a ((depth+1) mod dim) dim (cur ^ "1");
						compare_path right a ((depth+1) mod dim) dim (cur ^ "0"));;

		(* Check if it's a kd-tree *)
		let rec checkTree t depth dim = match t with
			| EmptyTree -> 0
			| Node(x,_,EmptyTree,EmptyTree) -> 0
			| Node(a,_,left,right) ->
				let new_depth = ((depth+1) mod dim) in
				match (left,right) with
					| EmptyTree,EmptyTree -> 0 
					| Node(x,_,_,_),EmptyTree ->
						if (x.(depth) > a.(depth)) then 1 + (checkTree left new_depth dim)
						else (checkTree left new_depth dim)
					| EmptyTree,Node(y,_,_,_) ->
						if (y.(depth) < a.(depth)) then 1 + (checkTree right new_depth dim)
						else (checkTree right new_depth dim)
					| (Node(x,_,_,_),Node(y,_,_,_)) ->
						let tmp = ref 0 in
						if (x.(depth) > a.(depth)) then tmp := !tmp + 1;
						if (y.(depth) < a.(depth)) then tmp := !tmp + 1;
						!tmp + (checkTree left new_depth dim) + (checkTree right new_depth dim);;

	(* Sanity test for NNS *)
	let sanity_test n =
		let array_test = (randomPoints (800,800) n) in
		let (tree_test,_) = constructKDT array_test in
		Graphics.set_color Graphics.red;
		drawTree tree_test;
		(* Choose a point *)
		let num = Random.int n in
		Graphics.set_color Graphics.blue;
		Graphics.draw_circle array_test.(num).(0) array_test.(num).(1) 10;
		(* Find the closest *)
		let (_,(closest,_)) = nns tree_test array_test.(num) int_tools 2 in
		Graphics.set_color Graphics.green;
		Graphics.draw_circle closest.(0) closest.(1) 10;
		(* Return the interssting stuff *)
		(array_test.(num),array_test);;

	(* Rebuild a tree to rebalance it *)
	(* TODO: Use R*-Trees to adress this *)
	let rebalance tree =
		(* Dump the whole tree points *)
		let rec dump t = match t with
			| EmptyTree -> []
			| Node(a,_,left,right) -> a::(ExtList.List.append (dump right) (dump left)) in
		let (ret,_) = constructKDT (Array.of_list (dump tree)) in
		ret;;

	(* Cross points *)
	(* let cross_points point_a point_b point_c =
		(* Geometry functions *)
		(* TODO: Use minus/mul/add *)
		let crossproduct a b c = ((c.(1) - a.(1)) * (b.(0) - a.(0)) - (c.(0) - a.(0)) * (b.(1) - a.(1))) in
		let dotproduct a b c = ((c.(0) - a.(0)) * (b.(0) - a.(0)) + (c.(1) - a.(1))*(b.(1) - a.(1))) in
		let squaredlengthba a b = ((b.(0) - a.(0))*(b.(0) - a.(0)) + (b.(1) - a.(1))*(b.(1) - a.(1))) in
		let squaredlength = squaredlengthba point_a point_b in
		let crossproduct = crossproduct point_a point_b point_c in
		let dotproduct = dotproduct point_a point_b point_c in
		if ((abs crossproduct) > 0) || (dotproduct < 0) || (dotproduct > squaredlength) then 
			false
		else true;; *)

	(* Test if two segments intersects *)
	(* Vector operations - 2D ONLY *)
	let add_vect vect_a vect_b = [|(vect_a.(0)+vect_b.(0));(vect_a.(1)+vect_b.(1))|];;
	let minus_vect vect_a vect_b = [|(vect_a.(0)-vect_b.(0));(vect_a.(1)-vect_b.(1))|];;
	let dot_vect vect_a vect_b = (vect_a.(0)*vect_b.(0))+(vect_a.(1)*vect_b.(1));;
	let mul_lambda_vect vect_a lambda = [|(vect_a.(0)*lambda);(vect_a.(1)*lambda)|];;
	let cross_vect vect_a vect_b = (vect_a.(0)*vect_b.(1))-(vect_a.(1)*vect_b.(0));;
	let lineSegmentsIntersect p p2 q q2 =
		let considerCollinearOverlapAsIntersect = true in
		let r = minus_vect p2 p in
		let s = minus_vect q2 q in
		let rxs = cross_vect r s in
		let qpxr = cross_vect (minus_vect p q) r in
		let ret = ref false in
		(* If r x s = 0 and (q - p) x r = 0, then the two lines are collinear. *)
		(if (rxs = 0) && (qpxr = 0) then
			(* 1. If either  0 <= (q - p) * r <= r * r or 0 <= (p - q) * s <= * s
				 then the two lines are overlapping *)
			if considerCollinearOverlapAsIntersect then
				begin
				let qpr = (dot_vect (minus_vect q p) r) in
				let pqs = (dot_vect (minus_vect p q) s) in
				if ((0 <= qpr && qpr <= (dot_vect r r)) || (0 <=  pqs && pqs <= (dot_vect s s))) then
					ret := true
				else
					(* 2. If neither 0 <= (q - p) * r = r * r nor 0 <= (p - q) * s <= s * s
						 then the two lines are collinear but disjoint.
						 No need to implement this expression, as it follows from the expression above. *)
					ret := false;
				end);
		(* 3. If r x s = 0 and (q - p) x r != 0, then the two lines are parallel and non-intersecting. *)
		(if not !ret then 
			(if (rxs = 0) && not (qpxr = 0) then
				ret := false
			else
				(let t = (float_of_int (cross_vect (minus_vect q p) s)) /. (float_of_int rxs) in
				let u = (float_of_int (cross_vect (minus_vect q p) r)) /. (float_of_int rxs) in
				let float_precision = 0.00000001 in
				let neg_float_precision = (-0.00000001) in
				(* 4. If r x s != 0 and 0 <= t <= 1 and 0 <= u <= 1
					 the two line segments meet at the point p + t r = q + u s. *)
				if (not (rxs = 0)) && (neg_float_precision <= t && t <= (1. +. float_precision))
						&& (neg_float_precision <= u && u <= (1. +. float_precision)) then
					(* An intersection was found. *)
					ret := true
				else
					(* 5. Otherwise, the two line segments are not parallel but do not intersect. *)
					ret := false;);););
		!ret;;

	let cross_points a d point =
		let x_vect = [|1;0|] in
		let x_m_vect = [|-1;0|] in
		let y_vect = [|0;1|] in
		let y_m_vect = [|0;-1|] in
		(lineSegmentsIntersect a d point x_vect) || (lineSegmentsIntersect a d point x_m_vect)
		|| (lineSegmentsIntersect a d point y_vect) || (lineSegmentsIntersect a d point y_m_vect);;

	(* Ray tracing *)
	(* RTX BOY ! *)
	(* Segment is defined as S = a + t*d where 0 <= t <= t_max *)
	let intersect_segment point_a point_b dim tre =
		(* Tell whether if poin is found *)
		let found = ref false in
		let float_precision = 0.00000001 in
		(* Make vector equation *)
		let a_init = Array.make dim 0. in
		let d_init = Array.make dim 0. in
		let d_int = Array.make dim 0 in
		for i = 0 to (dim-1) do
			a_init.(i) <- float_of_int point_a.(i);
			d_init.(i) <- float_of_int (point_b.(i) - point_a.(i));
			d_int.(i) <- (point_b.(i) - point_a.(i));
		done;
		let rec intersect_segment_aux a d tmax depth tr = match tr with
			| EmptyTree -> ()
			| Node(split_pt,_,left,right) ->
				(* Check if point was found already *)
				if not !found then
					begin
					(* Check if the point if aligned *)
					if cross_points point_a d_int split_pt then found := true
					else
						begin
						(* Compute depth for future recursive call *)
						let new_depth = (depth+1) mod dim in
						(* Figure out which child to recurse into first (0 = near, 1 = far) *)
						let split_value = (float_of_int split_pt.(depth)) in
						let (first,second) = if a.(depth) > split_value then
																	(right,left) else (left,right) in
						(* Segment parallel to splitting plane, visit near side only *)
						if (d.(depth) <= float_precision) && (d.(depth) >= (0. -. float_precision)) then
							intersect_segment_aux a d tmax new_depth first
						else
							begin
							(* Find t value for intersection between segment and split plane *)
							let t = (split_value -. a.(depth)) /. d.(depth) in
							(* Test if line segment straddles splitting plane *)
							if (t >= (0. -. float_precision)) && (t < tmax) then
								begin
								(* Yes, traverse near side first, then far side *)
								intersect_segment_aux a d t new_depth first;
								if not !found then
									(for i = 0 to (dim-1) do
										a.(i) <- a.(i) +. (t *. d.(i));
									done;
									intersect_segment_aux a d (tmax -. t) new_depth second;)							end
							else
								(*  No, so just traverse near side *)
								intersect_segment_aux a d tmax new_depth first;
							end
						end
					end in
		intersect_segment_aux a_init d_init 1. 0 tre;
		!found;;
	end