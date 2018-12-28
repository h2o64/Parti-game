(* R-Tree structure *)
let max_node_load = 7;;
let min_node_load = 3;;
type 'a point = 'a array;;
type ('a,'b) index = {
	pos : 'a point;
	data : 'b;
};;
type ('a,'b) tree =
	| Node of ('a Rect.rect ref * ('a,'b) tree ref) list
	| Leaf of ('a Rect.rect ref * ('a,'b) index) list
	| Empty;;

(* Operations *)
let add = (+);;
let minus = (-);;
let mul = ( * );;
let div = (/);;
let abs_f = abs;;
let zero = 0;;
let two = 2;;

(* Make empty nodes *)
let empty_node size = ((ref (Rect.empty zero size)),ref Empty);;
let empty_ref = ref Empty;;

(* Compute minimum enlargement *)
let enlargement rect_a rect_b =
		minus (Rect.volume (Rect.union !rect_a !rect_b) minus mul)
					(Rect.volume !rect_b minus mul);;
(* Return the node with smallest need of enlargement, an ordered list
	 (by enlargement) of the other nodes depending if they need enlargement or not
	 and keep track of the last best area *)
let rec partition_by_min_enlargement rect = function
	| ((r,_) as node)::[] ->
		(* Only one rectangle *)
		(node,[],(enlargement rect r))
	| ((r,_) as node)::ns ->
		(* Calculate needed enlargement *)
		let needed = enlargement rect r in
		(* Partition other nodes *)
		let (min,maxs,cur_enlargement) = partition_by_min_enlargement rect ns in
		(* Find the new best and updates the others *)
		if needed < cur_enlargement then (node,min::maxs,needed)
		else (min,node::maxs,cur_enlargement)
	| [] -> failwith "partition_by_min_enlargement: Empty list";;

(* List cross-product *)
let pairs_of_list xs =
  ExtList.List.concat (ExtList.List.map (fun x -> ExtList.List.map (fun y -> (x, y)) xs) xs);;

(* Quadratic-Cost splitting algorithm *)
(* Select two entries to be the first elements of the groups *)
let pickseeds ns =
	(* Compute the "inefficiency" of grouping given entries *)
	let cost (rect_a,_) (rect_b,_) =
		minus (minus (Rect.volume (Rect.union !rect_a !rect_b) minus mul)
		(Rect.volume !rect_a minus mul))
		(Rect.volume !rect_b minus mul) in
	(* Find the couple with largest cost *)
	let rec max_cost = function
		| (node1,node2)::[] -> (cost node1 node2),(node1,node2)
		| (node1,node2)::ns ->
			let worst_cost,worst_pair = max_cost ns in
			let cur_cost = cost node1 node2 in
			if cur_cost > worst_cost then cur_cost,(node1,node2)
			else (worst_cost,worst_pair)
		| [] -> failwith "pickseeds: Empty list" in
	(* Make all possible pairs *)
	let pairs = pairs_of_list ns in
	(* Return the chosen group *)
	let (_,ret) = max_cost pairs in
	ret;;
(* Select one remaining entry for classification in a group *)
let picknext group1 group2 ns =
	(* Compute the area required to be in a group *)
	let cost (e,_) = abs_f (minus (enlargement group1 e) (enlargement group2 e)) in
	(* Find the entry with the greatest preference to one group *)
	let rec preference = function
		| node::[] -> ((cost node),node)
		| node::ns ->
			let (best_cost,best_node) = preference ns in
			let cur_cost = cost node in
			(* Compare and update *)
			if cur_cost > best_cost then (cur_cost,node)
			else (best_cost,best_node)
		| [] -> failwith "picknext: Empty list" in
	let (_,ret) = preference ns in
	ret;;
(* Make the two groups based on the smallest area *)
let makegroups ns =
	let rec partition xs rect_xs ys rect_ys = function
		| [] -> (xs,rect_xs),(ys,rect_ys)
		| t ->
			(* Select the entry to assign *)
			let target_node = picknext rect_xs rect_ys t in
			let (target_rect,_) = target_node in
			(* TODO: Improve me *)
			(* Remove target node from the nodes list *)
			let ns = ExtList.List.filter ((!=) target_node) t in
			(* Compute the needed enlargements *)
			let enlargement_x = enlargement target_rect rect_xs in
			let enlargement_y = enlargement target_rect rect_ys in
			(* Adapt the paritions based on smallest erea *)
			if enlargement_x < enlargement_y then
				partition (target_node::xs) (ref (Rect.union !rect_xs !target_rect)) ys rect_ys ns
			else
				partition xs rect_xs (target_node::ys) (ref (Rect.union !rect_ys !target_rect)) ns in
	(* Pick the first entry for each group *)
	let (node1,node2) = pickseeds ns in
	let (e1,_) = node1 in
	let (e2,_) = node2 in
	(* Remove node1 and node2 from ns - TODO: Improve me *)
	let filtered_ns = (ExtList.List.filter (fun n -> n != node1 && n != node2) ns) in
	partition [node1] e1 [node2] e2 filtered_ns;;

(* Make a large rectangle from nodes *)
let rect_of_nodes ns = ref (Rect.unionMany (ExtList.List.map (fun (e,_) -> !e) ns));;

(* Select a leaf node in which to place a new index e *)
(* Prefer the places with the least amount of nodes *)
let rec chooseleaf rect_e e = function
	| Node ns ->
		begin
		(* Node case *)
		(* Find the subtree with the least enlargement to include e *)
		let ((_,min),maxs,_) = partition_by_min_enlargement rect_e ns in
		(* Descend until a leaf if reached *)
		match (chooseleaf rect_e e !min) with
			| new_min,(_,empty_tree) when empty_tree = empty_ref ->
				let new_nodes = new_min::maxs in
				let new_rect = rect_of_nodes new_nodes in
				(new_rect,ref (Node new_nodes)),(empty_node (Rect.get_dim !rect_e))
			| min_a,min_b ->
				(* Discriminate large nodes loads *)
				if ((ExtList.List.length maxs + 2) < max_node_load) then
					let new_nodes = min_a::min_b::maxs in
					let new_rect = rect_of_nodes new_nodes in
					(new_rect,ref (Node new_nodes)),(empty_node (Rect.get_dim !rect_e))
				else
					(* Split the node in two optimal groups *)
					let (a,rect_a),(b,rect_b) = makegroups (min_a::min_b::maxs) in
					(rect_a,ref (Node a)),(rect_b,ref (Node b))
			end
	| Leaf lf ->
		(* Leaf case *)
		(* Add element to the leaf *)
		let new_leaf = (rect_e,e)::lf in
		(* Discriminate large node loads *)
		if (ExtList.List.length new_leaf) < max_node_load then
			(rect_of_nodes new_leaf, ref (Leaf new_leaf)),(empty_node (Rect.get_dim !rect_e))
		else
			(* Split the leaf in two optimal groups *)
			let (a,rect_a),(b,rect_b) = makegroups new_leaf in
			(rect_a,(ref (Leaf a))),(rect_b,(ref (Leaf b)))
	| Empty -> (rect_e,(ref (Leaf [rect_e,e]))),(empty_node (Rect.get_dim !rect_e));;

(* Insertion of a node *)
let insert rect_e e t = match (chooseleaf rect_e e t) with
	| (_,a),(_,empty_tree) when empty_tree = empty_ref ->
		(* There is room *)
		!a
	| a,b ->
		(* Root split *)
		(Node [a;b]);;

(* Get all the rectangles intersecting rect *)
let filter_intersecting rect = ExtList.List.filter (fun (x,_) -> Rect.intersects !rect !x);;

(* Find all index records whose rectangles overlap a search rectangle *)
let rec find_leaf rect t = match t with
	| Node ns ->
		let intersecting = filter_intersecting rect ns in
		let found = ExtList.List.map (fun (_,n) -> find_leaf rect !n) intersecting in
		(ExtList.List.concat found);
	| Leaf lfs -> (filter_intersecting rect lfs)
	| Empty -> [];;

(* Get tree size *)
let rec size = function
  | Node ns ->
      let sub_sizes = ExtList.List.map (fun (_,n) -> size !n) ns in
      ExtList.List.fold_left (+) 0 sub_sizes
  | Leaf lfs -> ExtList.List.length lfs
  | Empty -> 0;;

(* Draw a tree *)
let rec draw t =
	(* Draw subtrees *)
	let rec draw_nodes ns = match ns with
		| (r,x)::t ->
			Graphics.set_color Graphics.blue;
			(* Rect.draw r; *)
			draw_nodes t;
			draw !x;
		| [] -> () in
	(* Draw leafs *)
	let rec draw_leaf ns = match ns with
		| (r,_)::t ->
			Graphics.set_color Graphics.red;
			Rect.draw !r;
			draw_leaf t;
		| [] -> () in
	match t with
		| Node ns -> draw_nodes ns
		| Leaf lfs -> draw_leaf lfs
		| Empty -> ();;

(* Get all the rectangles containing the point *)
let filter_contains point = ExtList.List.filter (fun (x,_) -> Rect.contains !x point);;

(* Find index record with for containing point *)
let rec find_point_list point t =
	match t with
		| Node ns ->
			let containing = filter_contains point ns in
			let found = ExtList.List.map (fun (_,n) -> find_point_list point !n) containing in
			ExtList.List.concat found;
		| Leaf lfs -> (filter_contains point lfs)
		| Empty -> [];;

(* Find the leaf with minimal area *)
let find_point point t =
	let point_list = find_point_list point t in
		if point_list = [] then failwith "find_point: Point can't be found";
	let init_nd = (ExtList.List.hd point_list) in
	let (init_rect,_) = init_nd in
	let ret_area = ref (Rect.volume !init_rect minus mul) in
	let ret = ref init_nd in
	(* Choose the minimum *)
	let rec find_point_aux l = match l with
		| ((rect,_) as nd)::q ->
			let vol = Rect.volume !rect minus mul in
			if vol < !ret_area then
				(ret_area := vol;
				ret := nd);
			find_point_aux q
		| [] -> () in
	find_point_aux point_list;
	!ret;;

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
	let rec build_tree l cur_tree = match l with
		| (r,idx)::t -> build_tree t (insert (ref r) idx cur_tree)
		| [] -> cur_tree in
	build_tree !indexes Empty;;

(* Split a node *)
let split node axis count t =
	let (target_rect,target_data) = node in
	let (rect1,rect2) = Rect.split target_rect axis div two in
	Rect.draw rect1;
	Rect.draw rect2;
	let tmp_tree = insert (ref rect1) {pos = (Rect.center rect1 add div two) ; data = count + 1} t in
	insert (ref rect2) {pos = (Rect.center rect2 add div two) ; data = count + 2} tmp_tree;;