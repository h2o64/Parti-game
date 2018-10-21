(* Graphic Library *)
#load "graphics.cma";;

module KDTrees :
  sig
		val getFormat : int -> int -> string
		val drawPoints : int array array -> unit
		val randomPoints : int * int -> int -> int array array
		val per_co_sort : 'a array array -> int -> int -> unit
		type 'a tree = EmptyTree | Node of 'a array * 'a tree * 'a tree
		val constructKDT : 'a array array -> int -> 'a tree
		val drawTree : int * int -> int tree -> unit
		val drawBorders : int tree -> unit
		val addTree : 'a array -> 'a tree -> int -> 'a tree
		val minimum_t : 'a tree -> int -> int -> int -> 'a array
		val maximum_t : 'a tree -> int -> int -> int -> 'a array
		val removeTree : 'a array -> 'a tree -> int -> 'a tree
		val uniformPoints : int * int -> int -> int -> int array array
		val distance_int : int array -> int array -> int -> int
		val distance_max_float : float
		val distance_float : float array -> float array -> int -> float
		type 'a distance_tools = {
			op_add : 'a -> 'a -> 'a;
			op_sub : 'a -> 'a -> 'a;
			op_mul : 'a -> 'a -> 'a;
			distance_f : 'a array -> 'a array -> int -> 'a;
			distance_inf : 'a;
		}
		val nns : 'a tree -> 'a array -> 'a distance_tools -> int -> 'a * 'a array
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

		(* Per-coordonate sort *)
		let per_co_sort arr depth size =
			(* Edit the array to keep index *)
			let arr_new = Array.make size (arr.(0).(depth),infinity,0) in
			for i = 0 to (size-1) do
				arr_new.(i) <- (arr.(i).(depth),infinity,i);
			done;
			(* Sort the coordonates *)
			Array.fast_sort compare arr_new;
			(* Change the original array *)
			let arr_c = Array.copy arr in
			for i = 0 to (size-1) do
				let (_,_,j) = arr_new.(i) in
				arr.(i)<-arr_c.(j);
			done;;

		(* Constuct a tree - TODO: Try avoiding the continuous call of per_co_sort *)
		let example = [| [|2;3|]; [|5;4|]; [|9;6|]; [|4;7|]; [|8;1|]; [|7;2|] |];;
		type 'a tree =
			| EmptyTree
			| Node of ('a array) * ('a tree) * ('a tree);;
		let constructKDT points dim =
			(* Do a in-place construction *)
			let rec constructKDT_aux cur_array depth =
				let n = Array.length cur_array in
				let next_dim = (depth+1) mod (dim-1) in
				per_co_sort cur_array depth n;
				match n with
					| 0 -> EmptyTree
					| 1 -> Node(cur_array.(0),EmptyTree,EmptyTree)
					| 2 -> Node(cur_array.(1),
									Node(cur_array.(0),EmptyTree,EmptyTree),
									EmptyTree)
					| 3 -> Node(cur_array.(2),
									Node(cur_array.(0),EmptyTree,EmptyTree),
									Node(cur_array.(1),EmptyTree,EmptyTree))
					| _ ->
							let median = (Array.length cur_array)/2 in
							Node(cur_array.(median),
								(constructKDT_aux (Array.sub cur_array 0 median) next_dim),
								(constructKDT_aux (Array.sub cur_array (median+1) (n-median-1)) next_dim)) in
			constructKDT_aux points 0;;
			
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
		let uniformPoints (h,w) per_line per_row =
			let ret = Array.make (((per_line+1)*(per_row+1))+1) [|0;0|] in
			let delta_x = w/per_line in
			let delta_y = h/per_row in
			let cur = ref 0 in
			for i = 0 to per_line do
				for j = 0 to per_row do
					ret.(!cur) <- [|i*delta_x;j*delta_y|];
					cur := !cur + 1;
				done;
			done;ret;;

		(* Squared distances *)
		let distance_max_int = max_int;;
		let distance_int a b length =
			let ret = ref 0 in
			for i = 0 to (length-1) do
				ret := !ret + ((a.(i) - b.(i))*(a.(i) - b.(i)))
			done;!ret;;
		let distance_max_float = infinity;;
		let distance_float a b length =
			let ret = ref 0. in
			for i = 0 to (length-1) do
				ret := !ret +. ((a.(i) -. b.(i))*.(a.(i) -. b.(i)))
			done;!ret;;

		(* Algebra toolpack *)
		type 'a distance_tools = {
				op_add : ('a -> 'a -> 'a);
				op_sub : ('a -> 'a -> 'a);
				op_mul : ('a -> 'a -> 'a);
				distance_f : ('a array -> 'a array -> int -> 'a);
				distance_inf : 'a };;
		let int_tools = {
				op_add = ( + );
				op_sub = ( - );
				op_mul = ( * );
				distance_f = distance_int;
				distance_inf = distance_max_int };;
		let float_tools = {
				op_add = ( +. );
				op_sub = ( -. );
				op_mul = ( *. );
				distance_f = distance_float;
				distance_inf = distance_max_float };;

		(* Nearest Neighbor Search *)
		let nns t target toolpack dim =
			let w = ref toolpack.distance_inf in
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
					let sq_x = toolpack.op_mul x.(depth) x.(depth) in
					let sq_target = toolpack.op_mul target.(depth) target.(depth) in
					if target.(depth) < x.(depth) then
						(if ((toolpack.op_sub sq_target !w) <= sq_x) then nns_aux left new_depth;
						if ((toolpack.op_add sq_target !w) > sq_x) then nns_aux right new_depth)
					else
						(if ((toolpack.op_add sq_target !w) > sq_x) then nns_aux right new_depth;
						if ((toolpack.op_sub sq_target !w) <= sq_x) then nns_aux left new_depth) in
			nns_aux t 0;
			(!w,!p);;

	end
