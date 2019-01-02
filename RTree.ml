module RTee :
	sig
		type ('a, 'b) bounded_item = {
		  mutable bb : 'a Rect.rect;
		  mutable item : 'b;
		}
		type ('a, 'b) leaf_data = { pos : 'a array; data : 'b; }
		type ('a, 'b) leaf = ('a, ('a, 'b) leaf_data) bounded_item
		type 'a node_data = { mutable nodes : 'a list; mutable hasLeaves : bool; }
		type ('a, 'b) node = ('a, 'b node_data) bounded_item
		type ('a, 'b) tree_struct =
		    Node of ('a, ('a, 'b) tree_struct) node
		  | Leaf of ('a, 'b) leaf
		  | Empty
		type ('a, 'b) tree = {
		  mutable root : ('a, 'b) tree_struct;
		  mutable size : int;
		  dimensions : int;
		}
		val insert : (int, 'a) tree -> int Rect.rect -> (int, 'a) leaf_data -> unit
		val grid : int -> int -> int -> (int, int) tree
		val draw_tree : (int, 'a) tree_struct -> unit
		val find_point : int array -> (int, 'a) tree -> (int, 'a) tree_struct
		val leaf_to_tuple : ('a, 'b) tree_struct -> 'a Rect.rect * 'a array * 'b
		val split : (int, 'a) tree_struct -> int -> int -> (int, int) tree -> unit
	end =
	struct
		(* Parameters *)
		let rtree_reinsert_p n = (30 * n) / 100;;
		let rtree_choose_subtree_p = 32;;
		let min_child_items = 32;;
		let max_child_items = 64;;

		(* Operations *)
		let add = (+);;
		let minus = (-);;
		let mul = ( * );;
		let div = (/);;
		let abs_f = abs;;
		let biggest = max_int;;
		let lowest = min_int;;
		let zero = 0;;
		let one = 1;;
		let two = 2;;
		let four = 4;;

		(* Define a tree *)
		type 'a point = 'a array;;
		type ('a,'b) bounded_item = { mutable bb : 'a Rect.rect ; mutable item : 'b };;
		type ('a,'b) leaf_data = {pos : 'a point;	data : 'b};;
		type ('a,'b) leaf = ('a, ('a, 'b) leaf_data) bounded_item
		type 'a node_data = { mutable nodes : 'a list ; mutable hasLeaves : bool };;
		type ('a,'b) node = ('a, 'b node_data) bounded_item;;
		type ('a,'b) tree_struct =
			| Node of ('a ,('a, 'b) tree_struct) node
			| Leaf of ('a, 'b) leaf
			| Empty;;
		type ('a, 'b) tree = {
			mutable root : ('a, 'b) tree_struct;
			mutable size : int;
			dimensions : int;
		};;

		(* Get backtrace *)
		let backtrace () =
			let tr = Printexc.get_callstack 5 in
			let log = Printexc.raw_backtrace_to_string tr in
			print_string log;;

		(* Assertions *)
		let assert_leaf leaf_t = match leaf_t with
			| Leaf lf -> lf
			| _ ->
				(backtrace ();
				failwith "assert_leaf: Not a leaf");;
		let assert_node node_t = match node_t with
			| Node nd -> nd
			| _ ->
				(backtrace ();
				failwith "assert_node: Not a node");;
		let assert_leaf_bb leaf_t = match leaf_t with
			| Leaf lf -> lf.bb
			| _ ->
				(backtrace ();
				failwith "assert_leaf_bb: Not a leaf");;
		let assert_node_bb node_t = match node_t with
			| Node nd -> nd.bb
			| _ ->
				(backtrace ();
				failwith "assert_node_bb: Not a node");;
		let assert_both_bb node_t = match node_t with
			| Node nd -> nd.bb
			| Leaf lf -> lf.bb
			| Empty ->
				(backtrace ();
				failwith "assert_both_bb: Empty tree");;
		let assert_is_leaf t = match t with	
			| Leaf _ -> true
			| _ ->
				(backtrace ();
				failwith "assert_is_leaf: Not a leaf");;

		(* Get the first p elements of a list *)
		let rec first_list l p cur = match l with
			| h::t -> if p = 0 then cur else first_list t (p-1) (h::cur)
			| [] -> cur;;

		(* Extract from a list *)
		let rec list_extract starting ending cur cur_l l to_the_end = match l with
			| h::t ->
				if cur < starting then list_extract starting ending (cur+1) cur_l t to_the_end
				else
					(if to_the_end then l
					else if cur > ending then cur_l
					else list_extract starting ending (cur+1) (h::cur_l) t to_the_end)
			| [] -> cur_l;;

		(* Sort items by area enlargement *)
		let partial_sort_by_area_enlargement target_volume l =
			let enlargement rect =
				(minus target_volume (Rect.volume rect minus mul)) in
			let items = List.map (fun x -> ((enlargement (assert_both_bb x)),x)) l in
			List.map snd (List.fast_sort compare items);;

		(* Find the minimum for a given compare function *)
		let rec min_list func l (cur_v,cur) = match l with
			| h::t ->
				let tmp = func (assert_both_bb h) in
				if tmp < cur_v then min_list func t (tmp,h)
				else min_list func t (cur_v,cur)
			| [] -> cur;;
		let sort_by_area_enlargement target_volume l =
			let enlargement rect = (minus target_volume (Rect.volume rect minus mul)) in
			let first = List.hd l in
			let ret = (min_list enlargement (List.tl l) ((enlargement (assert_both_bb first)),first)) in
			ret;;
		let sort_by_overlap_enlargement bb_e l =
			let overlap rect = Rect.overlap rect bb_e zero one minus mul in
			let first = List.hd l in
			let ret = (min_list overlap (List.tl l) (overlap (assert_both_bb first),first)) in
			ret;;

		(* Choose the appropriated subtree for insertion *)
		let chooseSubTree bb_e node =
			(* Assertion *)
			let n = assert_node node in
			(* bb_e volume *)
			let target_volume = (Rect.volume bb_e minus mul) in
			(* Returned thing *)
			let ret = ref None in
			(* If the child poiunts in t points to leaves *)
			let rec has_leaves tr = match tr with
				| Leaf lf -> false
				| Node nd ->
					(let next_t = (List.hd (nd.item.nodes)) in
					match next_t with
						| Leaf _ -> false
						| Empty -> false
						| Node next -> next.item.hasLeaves)			
				| Empty -> false in
			let chooseSubTree_leaf () =
				(* Determine the minimum overlap cost *)
				(if (max_child_items > (rtree_choose_subtree_p*2)/3) && ((List.length n.item.nodes) > rtree_choose_subtree_p) then
					((* Sort the rectangles in N in increasing order
						 of then area enlargement needed to include the
						 new data rectangle *)
					let grp_a = first_list (partial_sort_by_area_enlargement target_volume n.item.nodes) rtree_choose_subtree_p [] in
					(* From the items in A, considering all items in
						 N, choose the leaf whose rectangle needs least
					   overlap enlargement *)
					ret := Some (sort_by_overlap_enlargement bb_e grp_a))
				else
					((* Choose the leaf in N whose rectangle needs least
					 overlap enlargement to include the new data
					 rectangle Resolve ties by choosing the leaf
					 whose rectangle needs least area enlargement, then
					 the leaf with the rectangle of smallest area *)
					ret := Some (sort_by_overlap_enlargement bb_e n.item.nodes);)) in
			let chooseSubTree_nonLeaf () =
				(* Choose the leaf in N whose rectangle needs least
					 area enlargement to include the new data
					 rectangle. Resolve ties by choosing the leaf
					 with the rectangle of smallest area *)
				ret := Some (match (sort_by_area_enlargement target_volume n.item.nodes) with
					| Leaf lf -> Node {bb = (Rect.copyRect lf.bb) ; item = {nodes = [Leaf lf] ; hasLeaves = true}}
					| Node nd -> Node nd
					| Empty -> failwith "chooseSubTree_nonLeaf: Empty tree"); in
			(* Do if the next node has leaves *)
			if has_leaves node then chooseSubTree_leaf ()
			else chooseSubTree_nonLeaf ();
			(* Return the value *)
			match !ret with
				| None -> failwith "chooseSubTree: ret is None";
				| Some ret_node -> ret_node;;

		(* Adapt a rectangle from a group of nodes *)
		let rec distribution_box rect starting ending cur nodes = match nodes with
			| nd_t::t ->
				let nd_rect = assert_both_bb nd_t in
				(if cur < starting then distribution_box rect starting ending (cur+1) t
				else
					(if cur > ending then ()
					else
						(Rect.stretch rect nd_rect;
						distribution_box rect starting ending (cur+1) t);))
			| [] -> ();;

		(* Sort rectangles by edges *)
		let compare_first_edge axis node_a_t node_b_t =
			let rect_a,rect_b = (assert_both_bb node_a_t),(assert_both_bb node_b_t) in
			compare (Rect.getMinCorner rect_a).(axis) (Rect.getMinCorner rect_b).(axis);;
		let compare_second_edge axis node_a_t node_b_t =
			let rect_a,rect_b = (assert_both_bb node_a_t),(assert_both_bb node_b_t) in
			compare (Rect.getMaxCorner rect_a).(axis) (Rect.getMaxCorner rect_b).(axis);;

			(* This combines Split, ChooseSplitAxis, and ChooseSplitIndex into 
			 one function as an optimization (they all share data structures,
		 	 so it would be pointless to do all of that copying)
			 This returns a node, which should be added to the items of the
			 passed node's parent *)
		let split node_t =
			(* Assertition *)
			let node = assert_node node_t in
			let dimensions = Rect.getDimension node.bb in 
			let newNode = { bb = (Rect.reset dimensions biggest lowest) ; item = {nodes = [] ; hasLeaves = node.item.hasLeaves } } in
			(* Variables *)
			let n_items = List.length node.item.nodes in
			let distribution_count = (n_items - 2 * min_child_items + 1) in
			let split_axis = ref (dimensions + 1) in
			let split_edge = ref 0 in
			let split_index = ref 0 in
			let split_margin = ref zero in
			let sorted_edg = ref [] in
			(* Assertitions *)
			if not (n_items = (max_child_items + 1)) then failwith "split: Assertion 1 failed";
			if not (distribution_count > 0) then failwith "split: Assertion 2 failed";
			if not ((min_child_items+distribution_count-1) <= n_items) then failwith "split: Assertion 3 failed";
			(* Invoke ChooseSplitAxis to determine the axis,
				 perpendicular to which the split 1s performed
				 Invoke ChooseSplitIndex to determine the best
				 distribution into two groups along that axis *)
			for axis = 0 to (dimensions-1) do
				(* Initialize per-loop items *)
				let margin = ref zero in
				let overlap = ref zero in
				let dist_area = ref biggest in
				let dist_overlap = ref biggest in
				let dist_edge = ref 0 in
				let dist_index = ref 0 in
				(* Sort the items by the lower then by the upper
					 edge of their bounding box on this particular axis and 
					 determine all distributions as described . Compute S. the
					 sum of all margin-values of the different
					 distributions *)
				(* Lower edge == 0, Upper edge = 1 *)
				for edge = 0 to 1 do
					(* Sort the items by the correct key (upper edge, lower edge) *)
					sorted_edg :=
						if (edge = 0) then List.fast_sort (compare_first_edge axis) node.item.nodes
						else List.fast_sort (compare_second_edge axis) node.item.nodes;
					(* Distributions: pick a point m in the middle of the thing, call the left
						 R1 and the right R2. Calculate the bounding box of R1 and R2, then 
						 calculate the margins. Then do it again for some more points	*)
					for k = 0 to (distribution_count-1) do
						let area = ref zero in
						(* Calculate box for R1 and R2 *)
						(let r1 = (Rect.reset dimensions biggest lowest) in
						let r2 = (Rect.reset dimensions biggest lowest) in
						distribution_box r1 0 (min_child_items+k-1) 0 !sorted_edg;
						distribution_box r2 (min_child_items+k+1) n_items 0 !sorted_edg;
						(* Calculate the three values *)
						margin := add !margin (add (Rect.perimeter r1 minus add) (Rect.perimeter r2 minus add));
						area := add !area (add (Rect.volume r1 minus mul) (Rect.volume r2 minus mul));
						overlap := Rect.overlap r1 r2 zero one minus mul;
						(* Along the split axis, choose the distribution with the 
							 minimum overlap-value. Resolve ties by choosing the distribution
							 with minimum area-value. *)
						if (!overlap < !dist_overlap || (!overlap == !dist_overlap && !area < !dist_area)) then
							(* If so, store the parameters that allow us to recreate it at the end *)
							(dist_edge := edge;
							dist_index := min_child_items+k;
							dist_overlap := !overlap;
							dist_area := !area);)
					done;
				done;
				(* Choose the axis with the minimum S as split axis *)
				if (!split_axis = dimensions+1 || !split_margin > !margin) then
					(split_axis := axis;
					split_margin := !margin;
					split_edge := !dist_edge;
					split_index := !dist_index);
			done;
			((* Distribute the items into two groups *)
			let final_sort =
				if (!split_edge = 0) then List.fast_sort (compare_first_edge !split_axis) node.item.nodes
				else if (!split_axis <> (dimensions-1)) then List.fast_sort (compare_second_edge !split_axis) node.item.nodes
				else !sorted_edg (* failwith "split: Can't guess final_sort" *) in
			(* Distribute the end of the array to the new node, then erase them from the original node *)
			newNode.item.nodes<-((list_extract !split_index n_items 0 [] final_sort true));
			node.item.nodes<-((list_extract 0 (!split_index-1) 0 [] final_sort false));
			(* Adjust the bounding box for each 'new' node *)
			newNode.bb<-(Rect.reset dimensions biggest lowest);
			distribution_box newNode.bb 0 (n_items - !split_index+1) 0 newNode.item.nodes;
			node.bb<-(Rect.reset dimensions biggest lowest);
			distribution_box node.bb 0 (!split_index+1) 0 node.item.nodes;
			(Node newNode));;

		(* This routine is used to do the opportunistic reinsertion that the
			 R* algorithm calls for *)
		let rec reinsert t node_t =
			(* Assertion *)
			let node = assert_node node_t in
			let dimensions = Rect.getDimension node.bb in 
			let n_items = List.length node.item.nodes in
			let p = if (rtree_reinsert_p n_items) > 0 then (rtree_reinsert_p n_items)
							else 1 in
			(* For all M+l items of a node N, compute the distance
				 between the centers of their rectangles and the center
				 of the bounding rectangle of N *)
			if not (n_items = (max_child_items+1)) then
				failwith "reinsert: Fail Assertion 1";
			(* Sort the items in increasing order of their distances *)
			let compare_dist node_a_t node_b_t =
				let (rect_a,rect_b) = ((assert_both_bb node_a_t),(assert_both_bb node_b_t)) in
				compare (Rect.distanceFromCenter rect_a node.bb mul div add zero two)
								(Rect.distanceFromCenter rect_b node.bb mul div add zero two) in
			let sorted_list = List.fast_sort compare_dist node.item.nodes in
			(* Remove the last p items from N *)
			let removed_items = List.rev (list_extract (n_items-p) (n_items+1) 0 [] sorted_list true) in
			node.item.nodes<-List.rev (list_extract 0 (n_items-p-1) 0 [] sorted_list false); (* WARNING: Important rev *)
			(* Adjust the bounding rectangle of N *)
			node.bb<-(Rect.reset dimensions biggest lowest);
			distribution_box node.bb 0 (n_items-p+1) 0 node.item.nodes;
			(* In the sort, defined previously, starting with the 
				 minimum distance (= close reinsert), invoke Insert 
				 to reinsert the items *)
			let rec insert_everything l = match l with
				| lf_t::next ->
					(match (insertInternal t lf_t t.root false) with _ -> ();
					insert_everything next);
				| [] -> () in
			insert_everything removed_items

		(* Manage overflow *)
		and overflowTreatment t level_t firstInsert =
			(* Returned value *)
			let ret = ref None in
			(* If the level is not the root level AND this is the first
				 call of OverflowTreatment in the given level during the 
				 insertion of one data rectangle, then invoke Reinsert *)
			if (level_t <> t.root) && firstInsert then
				(reinsert t level_t;
				ret := Some Empty);
			if !ret = None then
				(let splitItem = split level_t in
				(* If OverflowTreatment caused a split of the root, create a new root *)
				if (level_t = t.root) then
					(let newRoot = {bb = (Rect.copyRect (assert_node_bb t.root)) ; item = { nodes = [splitItem;t.root] ; hasLeaves = false } } in
					Rect.stretch newRoot.bb (assert_node_bb splitItem);
					(* We are done *)
					t.root<-Node newRoot;
					ret := Some Empty)
				else
					(* Propagate it upwards *)
					ret := Some splitItem);
			(* Return the value *)
			match !ret with
				| None -> failwith "overflowTreatment: ret is None";
				| Some ret_node -> ret_node

		(* Insert nodes recursively *)
		and insertInternal t leaf_t node_t firstInsert =
			let (leaf,node) = ((assert_leaf leaf_t),(assert_node node_t)) in
			(* Returned value *)
			let ret = ref None in
			(* Adjust all covering rectangles in the insertion path
				 such that they are minimum bounding boxes
			   enclosing the children rectangles *)
			Rect.stretch node.bb leaf.bb;
			(if node.item.hasLeaves then
				(* If N has less than M items, accommodate E in N *)
				node.item.nodes<-leaf_t::node.item.nodes
			else
				(* Invoke ChooseSubtree. with the level as a parameter,
					 to find an appropriate node N, m which to place the
					 new leaf E
					 Of course, this already does all of that recursively. we just need to
					 determine whether we need to split the overflow or not *)
				(let tmp_node = insertInternal t leaf_t (chooseSubTree leaf.bb node_t) firstInsert in
				if tmp_node = Empty then
					ret := Some Empty
				else
					(* This gets joined to the list of items at this level *)
					node.item.nodes<-tmp_node::node.item.nodes););
			(* If nothing was done previously *)
			(if !ret = None then
				(* If N has M+1 items. invoke OverflowTreatment with the
					 level of N as a parameter [for reinsertion or split] *)
				if (List.length node.item.nodes) > max_child_items then
					ret := Some (overflowTreatment t node_t firstInsert)
				else
					ret := Some Empty;);
			(* Return the value *)
			match !ret with
				| None -> failwith "insertInternal: ret is None";
				| Some ret_node -> ret_node;;

		(* Single insert function, adds a new item to the tree *)
		let insert t bb_e e =
			let new_leaf = Leaf { bb = bb_e ; item = e } in
			if t.root = Empty then
				t.root <- Node { bb = bb_e ; item = { nodes = [new_leaf] ; hasLeaves = true }}
			else
				match (insertInternal t new_leaf t.root true) with _ -> ();
			t.size<-(t.size+1);;

		(* Get tree size *)
		let rec long_and l = match l with
			| h::t -> h && (long_and t)
			| [] -> true;;
		let data_nb = ref 0;;
		let rec check root t = match t with
		  | Node ns ->
		  	let len = (List.length ns.item.nodes) in
				let rest = List.map (check false) ns.item.nodes in
				if (len > max_child_items) then failwith "Too much";
				if (not root) && (len < min_child_items) then failwith "Not enough";
				(len <= max_child_items) && ((len >= min_child_items) || root) && (long_and rest)
		  | Leaf lfs -> data_nb := !data_nb + 1; true
		  | Empty -> true;;

		(* Create a grid tree *)
		let grid height width resolution =
			let count = ref 0 in
			let indexes = ref [] in
			(* Contruction *)
			let i = ref 0 in
			while (!i < (height-resolution)) do
				let j = ref 0 in
				while (!j < (width-resolution)) do
					let cur_rect = Rect.create [|!i;!j|] [|(!i+resolution);(!j+resolution)|] in
					let center = (Rect.center cur_rect add div two) in
					indexes := (cur_rect,{pos = center ; data = !count})::!indexes;
					count := !count + 1;
					j := !j + resolution;
				done;
				i := !i + resolution;
			done;
			(* Build the tree *)
			let ret_tree = { root = Empty ; size = 0 ; dimensions = 2 } in
			let rec build_tree l = match l with
				| (r,idx)::t -> insert ret_tree r idx; build_tree t
				| [] -> () in
			build_tree !indexes;
			ret_tree;;

		(* Create a random tree *)
		let cur_arr = ref [];;
		let random_tree height width count =
			let indexes = ref [] in
			(* Contruction *)
			for i = 1 to count do
					let (x,y) = (Random.int height),(Random.int width) in
					let cur_rect = Rect.create [|x;y|] [|x + (Random.int (height / 10));
																							 y + (Random.int (width / 10))|] in
					let center = (Rect.center cur_rect add div two) in
					indexes := (cur_rect,{pos = center ; data = i})::!indexes;
			done;
			cur_arr := !indexes;
			(* Build the tree *)
			let ret_tree = { root = Empty ; size = 0 ; dimensions = 2 } in
			let rec build_tree l = match l with
				| (r,idx)::t -> insert ret_tree r idx; build_tree t
				| [] -> () in
			build_tree (List.rev !indexes);
			ret_tree;;

		(* Guttman example tree *)
		let guttman scaling =
			let tr = { root = Empty ; size = 0 ; dimensions = 2 } in
			insert tr (Rect.create [|1*scaling;5*scaling|] [|4*scaling;7*scaling|]) { pos = [||] ; data = "R8"};
			insert tr (Rect.create [|6*scaling;1*scaling|] [|8*scaling;3*scaling|]) { pos = [||] ; data = "R9"};
			insert tr (Rect.create [|6*scaling;4*scaling|] [|8*scaling;6*scaling|]) { pos = [||] ; data = "R10"};
			insert tr (Rect.create [|9*scaling;0*scaling|] [|11*scaling;14*scaling|]) { pos = [||] ; data = "R11"};
			insert tr (Rect.create [|13*scaling;1*scaling|] [|14*scaling;10*scaling|]) { pos = [||] ; data = "R13"};
			insert tr (Rect.create [|12*scaling;5*scaling|] [|14*scaling;7*scaling|]) { pos = [||] ; data = "R14"};
			insert tr (Rect.create [|0*scaling;16*scaling|] [|2*scaling;18*scaling|]) { pos = [||] ; data = "R15"};
			insert tr (Rect.create [|3*scaling;11*scaling|] [|9*scaling;18*scaling|]) { pos = [||] ; data = "R16"};
			insert tr (Rect.create [|14*scaling;10*scaling|] [|21*scaling;14*scaling|]) { pos = [||] ; data = "R17"};
			insert tr (Rect.create [|16*scaling;8*scaling|] [|18*scaling;17*scaling|]) { pos = [||] ; data = "R18"};
			insert tr (Rect.create [|17*scaling;12*scaling|] [|20*scaling;15*scaling|]) { pos = [||] ; data = "R19"};
			tr;;

		(* Draw tree *)
		let rec apply_list f l = match l with
			| h::t -> (f h); apply_list f t
			| [] -> ();;
		let rec draw_tree t = match t with
			| Node nd ->
				let sub_f h =
					Graphics.set_color Graphics.red;
					Rect.draw (assert_both_bb h);
					draw_tree h in
					apply_list sub_f nd.item.nodes
			| Leaf lf ->
				Graphics.set_color Graphics.blue;
				Rect.draw lf.bb
			| Empty -> ();;

		(* Get all the rectangles containing the point *)
		let filter_contains point = List.filter (fun x -> Rect.contains (assert_both_bb x) point);;

		(* Find index record with for containing point *)
		let rec find_point_list point t =
			match t with
				| Node nd ->
					let containing = filter_contains point nd.item.nodes in
					let found = List.map (fun x -> find_point_list point x) containing in
					ExtList.List.concat found;
				| Leaf lfs -> if Rect.contains lfs.bb point then [t] else []
				| Empty -> [];;

		(* Find the leaf with minimal area *)
		let find_point point t =
			let point_list = find_point_list point t.root in
				if point_list = [] then failwith "find_point: Point can't be found";
			let init_nd = (ExtList.List.hd point_list) in
			let init_rect = (assert_both_bb init_nd) in
			let ret_area = ref (Rect.volume init_rect minus mul) in
			let ret = ref init_nd in
			(* Choose the minimum *)
			let rec find_point_aux l = match l with
				| nd::q ->
					let vol = Rect.volume (assert_both_bb nd) minus mul in
					if vol < !ret_area then
						(ret_area := vol;
						ret := nd);
					find_point_aux q
				| [] -> () in
			find_point_aux point_list;
			!ret;;

		(* Leaf to tuple *)
		let leaf_to_tuple t = match t with
			| Leaf fs -> (fs.bb,fs.item.pos,fs.item.data)
			| _ -> failwith "leaf_to_tuple: Not a leaf";;

		(* Split a node *)
		let split node axis count t =
			let (rect1,rect2) = Rect.split (assert_leaf_bb node) axis div two in
			Rect.draw rect1;
			Rect.draw rect2;
			insert t rect1 {pos = (Rect.center rect1 add div two) ; data = count + 1};
			insert t rect2 {pos = (Rect.center rect2 add div two) ; data = count + 2};;

		(* Benchmarks *)
		let time f x y =
			let start = Unix.gettimeofday ()
			in let res = f x y
			in let stop = Unix.gettimeofday ()
			in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start)
			in res;;
		let find_bench () =
			let tr = (grid 3000 3000 50) in
			for i = 0 to 20 do
				print_string "\ncount : ";
				print_int (500*i);
				print_string " | It took ";
				let start = Unix.gettimeofday () in
				for j = 0 to (500*i) do
					match (find_point [|(Random.int 2500);(Random.int 2500)|] tr) with _ -> ();
				done;
				let stop = Unix.gettimeofday () in
				print_float (stop -. start);
				Printf.printf "s";
			done;;
		let insert_bench () =
			let tr = (grid 3000 3000 50) in
			for i = 0 to 20 do
				print_string "\ncount : ";
				print_int (500*i);
				print_string " | It took ";
				let start = Unix.gettimeofday () in
				for j = 0 to (500*i) do
					let r1 = [|(Random.int 2500);(Random.int 2500)|] in
					let r2 = [|(Random.int 2500);(Random.int 2500)|] in
					insert tr (Rect.create r1 r2) { pos = r1 ; data = j };
				done;
				let stop = Unix.gettimeofday () in
				print_float (stop -. start);
				Printf.printf "s";
			done;;
		let find_bench () =
			let tr = (grid 750 750 50) in
			for i = 0 to 20 do
				print_string "\ncount : ";
				print_int (500*i);
				print_string " | It took ";
				let total_time = ref 0. in
				for j = 0 to (500*i) do
					let nd = (find_point [|(Random.int 700);(Random.int 700)|] tr) in
					let start = Unix.gettimeofday () in
					split nd (Random.int 2) j tr;
					let stop = Unix.gettimeofday () in
					total_time := !total_time +. (stop -. start);
				done;
				print_float !total_time;
				Printf.printf "s";
			done;;
	end