module CustomGraph :
  sig
		type 'a edge
		type ('a, 'b) info
		type ('a, 'b) graph
		val get_dim : ('a, 'b) graph -> int
		val get_count : ('a, 'b) graph -> int
		val create_edge : int -> 'a -> 'a edge
		val add_successor : 'a edge -> int -> unit
		val reset_successors : 'a edge -> unit
		val is_in_successors : 'a edge -> int -> bool
		val set_edge_maxstate : 'a edge -> int -> unit
		val set_edge_cost : 'a edge -> 'a -> unit
		val get_edge_succstate : 'a edge -> int
		val get_edge_successors : 'a edge -> int list
		val get_edge_maxstate : 'a edge -> int
		val get_edge_cost : 'a edge -> 'a
		val create_graph : int array -> int -> (unit -> 'a) -> int -> 'b -> (float, 'a) graph
		val draw_graph : (float, 'a) graph -> unit
		val adj : ('a, 'b) graph -> int -> int -> int -> bool
		val nei : ('a, 'b) graph -> int -> int -> (int * 'a edge) list
		val nei_n : ('a, 'b) graph -> int -> int -> int list
		val get_edg : ('a, 'b) graph -> int -> int -> int -> 'a edge list
		val set_edg : ('a, 'b) graph -> int -> int -> 'a edge -> int -> unit
		val add_edg : ('a, 'b) graph -> int -> int -> 'a edge -> int -> unit
		val add_bunch_edg : ('a, 'b) graph -> int -> (int * 'a edge) list -> int -> unit
		val rmv_edg : ('a, 'b) graph -> int -> int -> int -> unit
		val add_pt : (float, 'a) graph -> float array -> float Rect.rect -> (unit -> 'a) -> unit
		val rmv_nd : ('a, 'b) graph -> int -> unit
		val rmv_point : ('a, 'b) graph -> 'c -> int -> unit
		val get_node : ('a, 'b) graph -> int -> 'b
		val get_point : ('a, 'b) graph -> int -> 'a array
		val set_node : ('a, 'b) graph -> int -> 'b -> unit
		val find_node : (float, 'a) graph -> float array -> int
		val find_rect : (float, 'a) graph -> float array -> float Rect.rect
		val split : (float, 'a) graph -> (unit -> 'a) -> int -> int * int
		val bfs : ('a, int array) graph -> int -> int -> unit
		val dijkstra : (int, int array) graph -> int -> int -> unit
		val compute_path : ('a, int array) graph -> int -> int -> int list
		val shortest_path_bfs : ('a, int array) graph -> int -> int -> int -> int list * int
		val shortest_path_dijkstra : (int, int array) graph -> int -> int -> int -> int list * int
  end =
  struct
		(* Structures *)
		type 'a edge = {
			succstate : int;
			mutable successors : int list;
			mutable maxstate : int;
			mutable cost : 'a;
		}
		type ('a,'b) info = {
			point : 'a array;
			mutable neigh : (int * 'a edge) list array;
			mutable info : 'b;
		};;	
		type ('a,'b) graph = {
			dim : int;
			multiplicity : int;
			mutable count : int;
			mutable browse : ('a, int) RTree.tree;
			mutable data : (int, ('a,'b) info) Hashtbl.t;
		};;

		(* Get the dimension *)
		let get_dim graph = graph.dim;;

		(* Get graph size *)
		let get_count graph = graph.count;;

		(* Create an edge *)
		let create_edge b cost = {
			succstate = b;
			successors = [];
			maxstate = (-1);
			cost = cost
		};;

		(* Add successors to an edge *)
		let add_successor edg b = edg.successors<-b::edg.successors;;

		(* Reset successors *)
		let reset_successors edg = edg.successors<-[];;

		(* Look for a successor *)
		let is_in_successors edg b = List.mem b edg.successors;;

		(* Change some attributes of an edge *)
		let set_edge_maxstate edg b = edg.maxstate<-b;;
		let set_edge_cost edg x = edg.cost<-x;;

		(* Get attributes of an edge *)
		let get_edge_succstate edg = edg.succstate;;
		let get_edge_successors edg = edg.successors;;
		let get_edge_maxstate edg = edg.maxstate;;
		let get_edge_cost edg = edg.cost;;

		(* Create a graph from points *)
		let create_graph sizes resolution init_function multiplicity infinity_cost =
			(* Fill ratio for hash table *)
			let ratio = 8 in
			(* Create the tree *)
			let (tree,numeral,_) = (RTree.grid sizes resolution) in
			(* Make independent arrays *)
			let create_list x = [] in
			let create_nei i = Array.init multiplicity create_list in
			(* Create the empty hashtbl *)
			let length = Array.length numeral in
			let hashtbl = Hashtbl.create (length*ratio) in
			for i = 0 to (length-1) do
				Hashtbl.add hashtbl i {
					point = numeral.(i);
					info = (init_function ());
					neigh = (create_nei i);
				}
			done;
			{
				dim = (Array.length numeral.(0));
				count = length;
				multiplicity = multiplicity;
				browse = tree;
				data = hashtbl;
			};;

		(* Draw a graph *)
		let draw_graph graph = RTree.draw_tree graph.browse;;

		(* Adjacent - Test wether if x and y are adjacent in m *)
		let adj graph x y m = 
			let rec mem y l = match l with
				| [] -> false
				| (h,_)::t -> if (h = y) then true else mem y t in
			mem y (Hashtbl.find graph.data x).neigh.(m);;

		(* Neighbors - Find neight of x in m *)
		let nei graph x m = (Hashtbl.find graph.data x).neigh.(m);;
		let nei_n graph x m =
			List.map fst (Hashtbl.find graph.data x).neigh.(m);;

		(* Get value of an edge between x and y in m with *)
		let get_edg graph x y m =
			let rec get l = match l with
				| [] -> failwith "Edge not found"
				| (h,v)::t -> if (h = y) then v::(get t) else get t in
			get (Hashtbl.find graph.data x).neigh.(m);;

		(* Set value of an edge between x and y in m with *)
		let set_edg graph x y v m =
			let cur_value = (Hashtbl.find graph.data x) in
			let rec set l = match l with
				| [] -> failwith "Edge not found"
				| (h,_)::t -> if (h = y) then (y,v)::(set t) else set t in
			cur_value.neigh.(m) <- set cur_value.neigh.(m);
			Hashtbl.replace graph.data x cur_value;;

		(* Add an edge between x and y in m with *)
		let add_edg graph x y v m =
			let cur_value = (Hashtbl.find graph.data x) in
			cur_value.neigh.(m) <- ((y,v)::cur_value.neigh.(m));
			Hashtbl.replace graph.data x cur_value;;

		(* Add a bunch of edge at the same time *)
		let add_bunch_edg graph x ns m =
			let cur_value = (Hashtbl.find graph.data x) in
			cur_value.neigh.(m) <- (ExtList.List.append ns cur_value.neigh.(m));
			Hashtbl.replace graph.data x cur_value;;

		(* Remove an edge between x and y in m *)
		let rmv_edg graph x y m =
			let rec rmv_list l = match l with
				| [] -> []
				| (h,v)::t -> if h = y then (rmv_list t) else (h,v)::(rmv_list t) in
			let cur_value = (Hashtbl.find graph.data x) in
			cur_value.neigh.(m) <- rmv_list cur_value.neigh.(m);
			Hashtbl.replace graph.data x cur_value;;

		(* Add a point *)
		let add_pt graph point point_box init_function =
			(* Add to the tree *)
			graph.count <- graph.count + 1;
			RTree.insert graph.browse point_box (RTree.tuple_to_leaf_data point graph.count);
			let create_list k = [] in
			let create_empty_nei () = Array.init graph.multiplicity create_list in
			(* Add to the hashtbl *)
			Hashtbl.add graph.data (graph.count) {
				point = point;
				info = (init_function ());
				neigh = (create_empty_nei ());};;

		(* Remove a node *)
		let rmv_nd graph x =
			(* Wipe it's neighbor edges *)
			let rec rmv_nei l i = match l with
				| [] -> ()
				| (v,_)::t ->
					rmv_edg graph x v i;
					rmv_edg graph v x i;
					rmv_nei t i in
			for i = 0 to (graph.multiplicity-1) do
				rmv_nei (nei graph x i) i;
			done;
			(* Remove from the hashtbl *)
			Hashtbl.remove graph.data x;;

		(* Remove a point *)
		let rmv_point graph point nb =
			(* Wipe it's neighbor edges *)
			let rec rmv_nei l i = match l with
				| [] -> ()
				| (v,_)::t ->
					rmv_edg graph nb v i;
					rmv_edg graph v nb i;
					rmv_nei t i in
			for i = 0 to (graph.multiplicity-1) do
				rmv_nei (nei graph nb i) i;
			done;
			(* Add to the hashtbl *)
			Hashtbl.remove graph.data nb;;

		(* Get point info *)
		let get_node graph x = (Hashtbl.find graph.data x).info;;
		let get_point graph x = (Hashtbl.find graph.data x).point;;

		(* Set point info *)
		let set_node graph x info =
			let cur_value = (Hashtbl.find graph.data x) in
			cur_value.info <- info;
			Hashtbl.replace graph.data x cur_value;;

		(* Find the current cell *)
		let find_node graph x =
			let (_,_,nb) = (RTree.leaf_to_tuple (RTree.find_point x graph.browse)) in
			nb;;
		let find_rect graph x =
			let (rect,_,_) = (RTree.leaf_to_tuple (RTree.find_point x graph.browse)) in
			rect;;

		(* Split a given node *)
		let split graph init_function x =
			(* Get the rectangle *)
			let x_point = get_point graph x in
			let x_rtree_rect = find_rect graph x_point in
			let state1 = x in
			let state2 = graph.count in
			(* Insert the new rectangles in the RTree and grab their centers *)
			let (center1,center2) =
				RTree.split_rect x_rtree_rect graph.browse state1 state2 in
			(* Remove the node from the graph *)
			rmv_nd graph x;
			(* Create the new states *)
			let create_list k = [] in
			let create_empty_nei () = Array.init graph.multiplicity create_list in
			graph.count <- graph.count + 1;
			(* Add to the hashtbl with empty neiborhood *)
			Hashtbl.add graph.data state1 {
				point = center1;
				info = (init_function ());
				neigh = (create_empty_nei ());
			};
			Hashtbl.add graph.data state2 {
				point = center2;
				info = (init_function ());
				neigh = (create_empty_nei ());
			};
			(* Return the state numbers *)
			(state1,state2);;

		(* Breadth First Search Algorithm *)
		let bfs g s m =
			(* Structure of the header :
					0 -> color
					1 -> father
					2 -> dist
			*)
			let get_col u = (get_node g u).(0) in
			let get_dist u = (get_node g u).(2) in
			let set_var u i v =
				let info = (get_node g u) in
				info.(i)<-v; in
			let set_col u c = set_var u 0 c in
			let set_father u v = set_var u 1 v in
			let set_dist u v = set_var u 2 v in
			(* Variables *)
			let color_ref = Random.bits () (* Select a random color *)
			and q = Queue.create () in
			(* Initialisation *)
			if (get_col s) = color_ref then failwith "bfs: Wrong color";
			(* Set initial color *)
			set_col s color_ref;
			set_dist s 0;
			Queue.push s q;
			(* Work on neighboors *)
			let rec push_neigh u vs = match vs with
				| [] -> ()
				| (v,_) :: vs' ->
					if (get_col v) <> color_ref then (
						set_father v u;
						set_dist v ((get_dist u) + 1);
						set_col v color_ref;
						Queue.push v q
					);
					push_neigh u vs' in
			(* Actual loop *)
			while not (Queue.is_empty q) do
				let u = Queue.top q in
				push_neigh u (nei g u m);
				let _ = Queue.pop q in ()
			done;;

		(* Dijkstra Algorithm *)
		let dijkstra g r m =
			(* Structure of the header :
					0 -> color
					1 -> father
					2 -> dist
			*)
			let get_col u = (get_node g u).(0) in
			let get_dist u = (get_node g u).(2) in
			let set_var u i v =
				let info = (get_node g u) in
				info.(i)<-v; in
			let set_col u c = set_var u 0 c in
			let set_father u v = set_var u 1 v in
			let set_dist u v = set_var u 2 v in
			(* Variables *)
			let color_ref = Random.bits () in (* Select a random color *)
			if (get_col r) = color_ref then failwith "dijkstra: Wrong color";
			(* Make the priolist *)
			let get_prio x = get_dist x in
			let set_prio x v = set_dist x v in
			let order x y = (get_prio x) <= (get_prio y) in
			let f = PriorityQueue.make order in
			(* Functions *)
			let pop () =
				let ret = PriorityQueue.first f in
				PriorityQueue.remove_first f;
				ret in
			let update s k =
				let cur_prio = get_prio s in
				set_prio s k;
				if cur_prio < k then PriorityQueue.reorder_up f s
				else PriorityQueue.reorder_down f s in
			let insert s k =
				set_prio s k;
				PriorityQueue.add f s in
			(* Initialisation *)
			set_dist r 0;
			insert r 0;
			(* Work on neighbors *)
			let rec visit_neigh u l = match l with
				| [] -> ()
				| (v, edg) :: l' ->
					if ((get_col v) <> color_ref) && 
							((get_dist v) > (get_dist u) + (get_edge_cost edg) || (get_dist v) = -1) then (
						set_father v u;
						set_dist v ((get_dist u) + (get_edge_cost edg));
						if not (PriorityQueue.mem f v) then
							insert v (get_dist v)
						else
							update v (get_dist v);
					);
					visit_neigh u l' in
			while not (PriorityQueue.is_empty f) do
				let u = pop () in
				set_col u color_ref;
				visit_neigh u (nei g u m)
			done;;

		(* Compute path between two points given a father array *)
		let compute_path g u v =
			(* Actual computation *)
			let rec aux v pth =
				if v = u then u::pth
				else if (get_node g v).(1) = (-1) then [] (* failwith "compute_path" *)
				else
					let w = (get_node g v).(1) in aux w (v :: pth) in
			aux v [];;

		(* Shortest path between u and v in g for m using bfs *)
		let shortest_path_bfs g u v m =
			bfs g u m;
			(compute_path g u v, (get_node g v).(2));;

		(* Shortest path between u and v in g for m using dijkstra *)
		let shortest_path_dijkstra g u v m =
			dijkstra g u m;
			(compute_path g u v, (get_node g v).(2));;
	end
