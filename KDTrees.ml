(* Graphic Library *)
#load "graphics.cma";;

module KDTrees :
  sig
		type 'a tree
		val constructKDT : 'a array array -> 'a tree * 'a array array
		val drawTree : float tree -> unit
		val addTree : 'a array -> 'a tree -> int -> int -> 'a tree
		val removeTree : 'a array -> 'a tree -> int -> 'a tree
		val nns : float tree -> float array -> int -> float * (float array * int)
		val knns :  float tree -> float array -> int -> int -> (float * (float array * int)) array
		val rebalance : 'a tree -> 'a tree
		val intersect_segment : float array -> float array -> int -> float tree -> bool * float array
  end =

  struct
		(* Get the right window format *)
		let getFormat height width =
			let s_height = string_of_int height in
			let s_width = string_of_int width in
			String.concat "" [" ";s_height;"x";s_width];;

		(* Draw a set of 2D points *)
		let drawPoints points =
			(* 2D Only function *)
			if (Array.length points.(0) <> 2) then failwith "drawPoints: 2D Only";
			(* Sorts points *)
			let n = Array.length points in
			Array.fast_sort compare points;
			(* Make the window - Force (0,0) to be the origin *)
			let (max_x,max_y) = (to_int points.(n-1).(0),to_int points.(n-1).(1)) in
			Graphics.open_graph (getFormat max_x max_y);
			(* Fill the window *)
			for i = 0 to (n-1) do
				Graphics.plot (to_int points.(i).(0)) (to_int points.(i).(1));
			done;;

		(* Get a random 2D points set *)
		let randomPoints (h,w) n =
			let ret = Array.make (n+1) [|zero;zero|] in
			for i = 0 to n do
				ret.(i) <- [|(Random.float w);(Random.float h)|];
			done;ret;;

		(* Constuct a tree *)
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
			(* 2D Only function *)
			print_string "drawTree: 2D Only";
			match t with
				| EmptyTree -> ()
				| Node(point,_,left,right) -> ((Graphics.plot (to_int point.(0)) (to_int point.(1)));
														 drawTree left;
														 drawTree right);;

		(* Draw borders for a 2D Tree *)
		let drawBorders t =
			(* 2D Only function *)
			print_string "drawTree: 2D Only";
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
							(Graphics.moveto (to_int point.(0)) 0;
							Graphics.lineto (to_int point.(0)) (to_int median_point.(1)))
						else
							(Graphics.moveto (to_int point.(0)) h;
							Graphics.lineto (to_int point.(0)) (to_int median_point.(1))))
					else
						(Graphics.set_color Graphics.blue;
						if side = 0 then
							(Graphics.moveto 0 (to_int point.(1));
							Graphics.lineto (to_int median_point.(0)) (to_int point.(1)))
						else
							(Graphics.moveto (to_int median_point.(0)) (to_int point.(1));
							Graphics.lineto w (to_int point.(1))););
					drawBorders_aux left (i+1) point 0;
					drawBorders_aux right (i+1) point 1 in
			drawBorders_aux t 0 [|of_int h;of_int w|] 0;;

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
			let ret = Array.make (((per_line+1)*(per_row+1))+1) [|zero;zero|] in
			let delta_x = w/per_line in
			let delta_y = h/per_row in
			let cur = ref 0 in
			for i = 0 to per_line do
				for j = 0 to per_row do
					ret.(!cur) <- [|of_int (i*delta_x);of_int (j*delta_y)|];
					cur := !cur + 1;
				done;
			done;
			(* Random-izer 3000 *)
			if random then
				(for i = 0 to (per_line*per_row)/3 do
					swap ret (Random.int (per_line*per_line)) (Random.int (per_line*per_line));
				done;ret)
			else ret;;

		(* Nearest Neighbor Search *)
		let nns t target dim =
			(* Algorithm data *)
			let w = ref biggest in
			let p = ref (target,(-1)) in
			let target_c = ref (-1) in
			let rec nns_aux cur_t depth = match cur_t with
				| EmptyTree -> ()
				| Node(x,c,left,right) ->
					let new_depth = ((depth+1) mod dim) in
					(* Check is the point is better than the best *)
					let new_w = distance_f target x dim in
					(* Remenber the target count just in case *)
					if (x = target) then
						target_c := c
					else
						(if (new_w < !w) then
							(w := new_w;
							p := (x,c)));
					(* Visit subtrees *)
					if target.(depth) <= x.(depth) then
						(nns_aux left new_depth;
						if (add target.(depth) !w) >= x.(depth) then nns_aux right new_depth)
					else
						(nns_aux right new_depth;
						if (minus target.(depth) !w) <= x.(depth) then nns_aux left new_depth) in

			nns_aux t 0;
			if (!p = (target,(-1))) then (zero,(target,!target_c)) else (!w,!p);;

		(* k-Nearest Neighbor Search - TODO: Use a stack *)
		let knns t target k dim =
			(* Algorithm data *)
			let w = ref biggest in
			let ret = ref [] in
			let target_c = ref (-1) in
			let rec nns_aux cur_t depth = match cur_t with
				| EmptyTree -> ()
				| Node(x,c,left,right) ->
					let new_depth = ((depth+1) mod dim) in
					(* Check is the point is better than the best *)
					let new_w = distance_f target x dim in
					(* Remenber the target count just in case *)
					if (x = target) then
						target_c := c
					else
						(* Add it to the list *)
						(ret := (new_w,(x,c))::!ret;
						if (new_w < !w) then w := new_w);
					(* Visit subtrees *)
					if target.(depth) <= x.(depth) then
						(nns_aux left new_depth;
						if (add target.(depth) !w) >= x.(depth) then nns_aux right new_depth)
					else
						(nns_aux right new_depth;
						if (minus target.(depth) !w) <= x.(depth) then nns_aux left new_depth) in
			nns_aux t 0;
			(* Return the best k *)
			let ret_a = (Array.of_list !ret) in
			Array.fast_sort compare ret_a;
			let goal_length = if (Array.length ret_a) > k then k else (Array.length ret_a) in
			let ret = Array.sub ret_a 0 goal_length in
			if ret = [||] then [|(zero,(target,!target_c))|]
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
		(* 2D Only function *)
		print_string "sanity_test: 2D Only";
		let array_test = (randomPoints (800.,800.) n) in
		let (tree_test,_) = constructKDT array_test in
		Graphics.set_color Graphics.red;
		drawTree tree_test;
		(* Choose a point *)
		let num = Random.int n in
		Graphics.set_color Graphics.blue;
		Graphics.draw_circle (to_int array_test.(num).(0)) (to_int array_test.(num).(1)) 10;
		(* Find the closest *)
		let (_,(closest,_)) = nns tree_test array_test.(num) 2 in
		Graphics.set_color Graphics.green;
		Graphics.draw_circle (to_int closest.(0)) (to_int closest.(1)) 10;
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

	(* Test if two segments intersects *)
	(* Vector operations - 2D ONLY *)
	let add_vect vect_a vect_b = [|(add vect_a.(0) vect_b.(0));(add vect_a.(1) vect_b.(1))|];;
	let minus_vect vect_a vect_b = [|(minus vect_a.(0) vect_b.(0));(minus vect_a.(1) vect_b.(1))|];;
	let dot_vect vect_a vect_b = add (mul vect_a.(0) vect_b.(0)) (mul vect_a.(1) vect_b.(1));;
	let mul_lambda_vect vect_a lambda = [|(mul vect_a.(0) lambda);(mul vect_a.(1) lambda)|];;
	let cross_vect vect_a vect_b = minus (mul vect_a.(0) vect_b.(1)) (mul vect_a.(1) vect_b.(0));;
	let lineSegmentsIntersect p p2 q q2 =
		(* 2D Only function *)
		if (Array.length p) <> 2 then failwith  "lineSegmentsIntersect: 2D Only";
		let considerCollinearOverlapAsIntersect = true in
		let r = minus_vect p2 p in
		let s = minus_vect q2 q in
		let rxs = cross_vect r s in
		let qpxr = cross_vect (minus_vect p q) r in
		let ret = ref false in
		let ret_point = ref [||] in
		(* If r x s = 0 and (q - p) x r = 0, then the two lines are collinear. *)
		(if (is_0 rxs) && (is_0 qpxr) then
			(* 1. If either  0 <= (q - p) * r <= r * r or 0 <= (p - q) * s <= * s
				 then the two lines are overlapping *)
			if considerCollinearOverlapAsIntersect then
				begin
				let qpr = (dot_vect (minus_vect q p) r) in
				let pqs = (dot_vect (minus_vect p q) s) in
				if (((is_pos qpr) && qpr <= (dot_vect r r)) || ((is_pos pqs) && pqs <= (dot_vect s s))) then
					ret := true
				else
					(* 2. If neither 0 <= (q - p) * r = r * r nor 0 <= (p - q) * s <= s * s
						 then the two lines are collinear but disjoint.
						 No need to implement this expression, as it follows from the expression above. *)
					ret := false;
				end);
		(* 3. If r x s = 0 and (q - p) x r != 0, then the two lines are parallel and non-intersecting. *)
		(if not !ret then 
			(if (is_0 rxs) && not (is_0 qpxr) then
				ret := false
			else
				(let t = div ((cross_vect (minus_vect q p) s)) rxs in
				let u = div ((cross_vect (minus_vect q p) r)) rxs in
				(* 4. If r x s != 0 and 0 <= t <= 1 and 0 <= u <= 1
					 the two line segments meet at the point p + t r = q + u s. *)
				if (not (is_0 rxs)) && (neg_precision <= t && t <= (add one precision))
						&& (neg_precision <= u && u <= (add one precision)) then
					(* We can calculate the intersection point using either t or u. *)
					(ret_point := add_vect p (mul_lambda_vect r t);
					(* An intersection was found. *)
					ret := true)
				else
					(* 5. Otherwise, the two line segments are not parallel but do not intersect. *)
					ret := false;);););
		(!ret,!ret_point);;

	let cross_points a d point =
		let x_vect = [|one;zero|] in
		let x_m_vect = [|minus zero one;zero|] in
		let y_vect = [|zero;one|] in
		let y_m_vect = [|zero;minus zero one|] in
		let rec big_or l = match l with
			| dir::t ->
				let (inter,po) = (lineSegmentsIntersect a d point dir) in
				if inter then (true,po)
				else big_or t
			| [] -> (false,[||]) in
		big_or [x_vect;y_vect;x_m_vect;y_m_vect];;

	(* Ray tracing *)
	(* RTX BOY ! *)
	(* Segment is defined as S = a + t*d where 0 <= t <= t_max *)
	let intersect_segment point_a point_b dim tre =
		(* Tell whether if poin is found *)
		let found = ref false in
		let found_point = ref [||] in
		(* Make vector equation *)
		let d_init = Array.make dim zero in
		for i = 0 to (dim-1) do
			d_init.(i) <- (minus point_b.(i) point_a.(i));
		done;
		let rec intersect_segment_aux a d tmax depth tr = match tr with
			| EmptyTree -> ()
			| Node(split_pt,_,left,right) ->
				(* Check if point was found already *)
				if not !found then
					begin
					(* Check if the point if aligned *)
					let (my_bool,my_point) = cross_points point_a d_init split_pt in
					if my_bool then
						(found := true;
						found_point := my_point)
					else
						begin
						(* Compute depth for future recursive call *)
						let new_depth = (depth+1) mod dim in
						(* Figure out which child to recurse into first (0 = near, 1 = far) *)
						let split_value = split_pt.(depth) in
						let (first,second) = if a.(depth) > split_value then
																	(right,left) else (left,right) in
						(* Segment parallel to splitting plane, visit near side only *)
						if (is_0 d.(depth)) then
							intersect_segment_aux a d tmax new_depth first
						else
							begin
							(* Find t value for intersection between segment and split plane *)
							let t = div (minus split_value a.(depth)) d.(depth) in
							(* Test if line segment straddles splitting plane *)
							if (is_pos t) && (t < tmax) then
								begin
								(* Yes, traverse near side first, then far side *)
								intersect_segment_aux a d t new_depth first;
								if not !found then
									(for i = 0 to (dim-1) do
										a.(i) <- add a.(i) (mul t d.(i));
									done;
									intersect_segment_aux a d (minus tmax t) new_depth second;)
								end
							else
								(*  No, so just traverse near side *)
								intersect_segment_aux a d tmax new_depth first;
							end
						end
					end in
		intersect_segment_aux (Array.copy point_a) d_init one 0 tre;
		(!found,!found_point);;
	end
