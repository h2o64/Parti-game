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
		val removeTree : 'a array -> 'a tree -> int -> 'a tree
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

		(* Better removal in kd-tree *)
		let removeTree target t dim =
			(* Find the max/min for the targetted depth *)
			let rec extremum_t_min cut_t cur_depth depth = match cut_t with
				| EmptyTree -> failwith "extremum_t_min: Error"
				| Node(x,left,right) ->
					let new_depth = ((cur_depth+1) mod dim) in
					if (cur_depth = depth) then
						if (left = EmptyTree) then x
						else extremum_t_min left new_depth depth
					else
						min (min (extremum_t_min left new_depth depth)
								(extremum_t_min right new_depth depth)) in
			let rec extremum_t_max cut_t cur_depth depth = match cut_t with
				| EmptyTree -> failwith "extremum_t_max: Error"
				| Node(x,left,right) ->
					let new_depth = ((cur_depth+1) mod dim) in
					if (cur_depth = depth) then
						if (right = EmptyTree) then x
						else extremum_t_max right new_depth depth
					else
						max (extremum_t_max left new_depth depth)
								(extremum_t_max right new_depth depth) in
			(* Find the optimal replcament *)
			let rec find_replacement sub_t depth = match sub_t with
				| EmptyTree -> failwith "find_replacement: Error EmptyTree";
				| Node(x,EmptyTree,right) -> extremum_t_min right depth depth
				| Node(x,left,_) -> extremum_t_max left depth depth in
			(* Find the node to replace *)
			let rec search_and_replace cur_t target_cur depth = match cur_t with
				| EmptyTree -> EmptyTree
				| Node(x,EmptyTree,EmptyTree) ->
					if (x = target_cur) then EmptyTree
					else cur_t (* It's a leaf *)
				| Node(x,left,right) ->
					let new_depth = ((depth+1) mod dim) in
					if (target_cur = x) then
						let rep = find_replacement t depth in
						(* Recursively remove the replacement *)
						if (rep.(depth) < x.(depth)) then
							Node(rep,(search_and_replace left rep new_depth),right)
						else
							Node(rep,left,(search_and_replace right rep new_depth));
					else
							Node(x,(search_and_replace left target_cur new_depth),
										 (search_and_replace right target_cur new_depth)) in
			
			search_and_replace t target 0;
	end

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
