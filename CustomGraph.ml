module CustomGraph :
  sig
	type node
	type 'a point
	type 'a edge
	type ('a, 'b, 'c) info
	type ('a, 'b, 'c) graph
	val create_graph : int -> int -> int -> (unit -> 'a) -> int -> (int, 'b, 'a) graph
	val adj : ('a, 'b, 'c) graph -> node -> node -> int -> bool
	val nei : ('a, 'b, 'c) graph -> node -> int -> 'b edge list
	val get_edg : ('a, 'b, 'c) graph -> node -> node -> int -> 'b
	val set_edg : ('a, 'b, 'c) graph -> node -> node -> 'b -> int -> unit
	val add_edg : ('a, 'b, 'c) graph -> node -> node -> 'b -> int -> unit
	val add_bunch_edg : ('a, 'b, 'c) graph -> node -> 'b edge list -> int -> unit
	val rmv_edg : ('a, 'b, 'c) graph -> node -> node -> int -> unit
	val add_pt : (int, 'a, 'b) graph -> int point -> int Rect.rect -> (unit -> 'b) -> unit
	val get_node : ('a, 'b, 'c) graph -> node -> 'c
	val set_node : ('a, 'b, 'c) graph -> node -> 'c -> unit
	val find_node : (int, 'a, 'b) graph -> int array -> node
	val bfs : ('a, 'b, node array) graph -> node -> int -> unit
	val dijkstra : ('a, int, node array) graph -> node -> int -> unit
	val compute_path : ('a, 'b, node array) graph -> node -> node -> node list
	val shortest_path_bfs : ('a, 'b, node array) graph -> node -> node -> int -> node list * node
	val shortest_path_dijkstra : ('a, int, node array) graph -> node -> node -> int -> node list * node
  end =
  struct
		(* Structures *)
		type node = int;;
		type 'a point = 'a array;;
		type 'a edge = node * 'a;;
		type ('a,'b,'c) info = {
			point : 'a point;
			mutable neigh : 'b edge list array;
			mutable info : 'c;
		};;	
		type ('a,'b,'c) graph = {
			dim : int;
			multiplicity : int;
			mutable count : int;
			mutable browse : ('a, node) RTree.tree;
			mutable data : (node, ('a,'b,'c) info) Hashtbl.t;
		};;

		(* Create a graph from points *)
		let create_graph height width resolution init_function multiplicity =
			(* Fill ratio for hash table *)
			let ratio = 4 in
			(* Make independent arrays *)
			let create_list x = [] in
			let create_empty_nei () = Array.init multiplicity create_list in
			(* Create the tree *)
			let (tree,numeral) = (RTree.grid height width resolution) in
			(* Create the empty hashtbl *)
			let length = Array.length numeral in
			let hashtbl = Hashtbl.create (length*ratio) in
			for i = 0 to (length-1) do
				Hashtbl.add hashtbl i {
					point = numeral.(i);
					info = (init_function ());
					neigh = (create_empty_nei ());}
			done;
			{
				dim = 2;
				count = length;
				multiplicity = multiplicity;
				browse = tree;
				data = hashtbl;
			};;

		(* Adjacent - Test wether if x and y are adjacent in m *)
		let adj graph x y m = 
			let rec mem y l = match l with
				| [] -> false
				| (h,_)::t -> if (h = y) then true else mem y t in
			mem y (Hashtbl.find graph.data x).neigh.(m);;

		(* Neighbors - Find neight of x in m *)
		let nei graph x m = (Hashtbl.find graph.data x).neigh.(m);;

		(* Get value of an edge between x and y in m with *)
		let get_edg graph x y m =
			let rec get l = match l with
				| [] -> failwith "Edge not found"
				| (h,v)::t -> if (h = y) then v else get t in
			get (Hashtbl.find graph.data x).neigh.(m);;

		(* Set value of an edge between x and y in m with *)
		let set_edg graph x y v m =
			let cur_value = (Hashtbl.find graph.data x) in
			let rec set l = match l with
				| [] -> failwith "Edge not found"
				| (h,_)::t -> if (h = y) then (y,v)::t else set t in
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
				| (h,v)::t -> if h = y then t else (h,v)::(rmv_list t) in
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
			for i = 0 to graph.multiplicity do
				rmv_nei (nei graph x i) i;
			done;
			(* Remove from the hashtbl *)
			Hashtbl.remove graph.data x;;

		(* Remove a point *)
		let rmv_point graph (point : 'a point) nb =
			(* Wipe it's neighbor edges *)
			let rec rmv_nei l i = match l with
				| [] -> ()
				| (v,_)::t ->
					rmv_edg graph nb v i;
					rmv_edg graph v nb i;
					rmv_nei t i in
			for i = 0 to graph.multiplicity do
				rmv_nei (nei graph nb i) i;
			done;
			(* Add to the hashtbl *)
			Hashtbl.remove graph.data nb;;

		(* Get point info *)
		let get_node graph x = (Hashtbl.find graph.data x).info;;

		(* Set point info *)
		let set_node graph x info =
			let cur_value = (Hashtbl.find graph.data x) in
			cur_value.info <- info;
			Hashtbl.replace graph.data x cur_value;;

		(* Find the current cell *)
		let find_node graph x =
			let (_,_,nb) = (RTree.leaf_to_tuple (RTree.find_point x graph.browse)) in
			(nb : node);;

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
			let n = Hashtbl.length g.data in
			let f = PriorityQueue.create n r in
			(* Initialisation *)
			set_dist r 0;
			PriorityQueue.push (r, 0) f;
			(* Work on neighbors *)
			let rec visit_neigh u l = match l with
				| [] -> ()
				| (v, d) :: l' ->
					if ((get_col v) <> color_ref) && 
							((get_dist v) > (get_dist u) + d || (get_dist v) = -1) then (
						set_father v u;
						set_dist v ((get_dist u) + d);
						if not (PriorityQueue.is_in v f) then
							PriorityQueue.push (v, (get_dist v)) f
						else
							PriorityQueue.decrease_prio (v, (get_dist v)) f;
					);
					visit_neigh u l' in
			while not (PriorityQueue.is_empty f) do
				let (u,_) = PriorityQueue.pop f in
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