module CustomGraph :
  sig
		type 'a point
		type ('a, 'b) edge
		type ('a, 'b, 'c) info
		type ('a, 'b, 'c) graph
		val create_graph : 'a point array -> (unit -> 'b) -> int -> ('a, 'c, 'b) graph
		val adj : ('a, 'b, 'c) graph -> 'a point -> 'a point -> int -> bool
		val nei : ('a, 'b, 'c) graph -> 'a point -> int -> ('a, 'b) edge list
		val get_edg : ('a, 'b, 'c) graph -> 'a point -> 'a point -> int -> 'b
		val set_edg : ('a, 'b, 'c) graph -> 'a point -> 'a point -> 'b -> int -> unit
		val add_edg : ('a, 'b, 'c) graph -> 'a point -> 'a point -> 'b -> int -> unit
		val add_bunch_edg : ('a, 'b, 'c) graph -> 'a point -> ('a, 'b) edge list -> int -> unit
		val rmv_edg : ('a, 'b, 'c) graph -> 'a point -> ('a, 'b) edge -> int -> unit
		val add_pt : ('a, 'b, 'c) graph -> 'a point -> (unit -> 'c) -> unit
		val rmv_pt : ('a, 'b, 'c) graph -> 'a point -> unit
		val get_pt : ('a, 'b, 'c) graph -> 'a point -> 'c
		val set_pt : ('a, 'b, 'c) graph -> 'a point -> 'c -> unit
		val find_cell : ('a, 'b, 'c) graph -> 'a point -> 'a KDTrees.distance_tools -> 'a point
		val rebalance : ('a, 'b, 'c) graph -> unit
		val bfs : (int, 'a, int array) graph -> int point -> int -> unit
		val dijkstra : (int, int, int array) graph -> int point -> int -> unit
		val compute_path : (int, 'a, int array) graph -> int point -> int point -> int point list
		val shortest_path_bfs : (int, 'a, int array) graph -> int point -> int point -> int -> int point list * int
		val shortest_path_dijkstra : (int, int, int array) graph -> int point -> int point -> int -> int point list * int
  end =
  struct
		(* Structures *)
		type 'a point = 'a array;;
		type ('a,'b) edge = 'a point * 'b;;
		type ('a,'b,'c) info = {
			mutable neigh : ('a,'b) edge list array;
			mutable info : 'c;
		};;	
		type ('a,'b,'c) graph = {
			dim : int;
			multiplicity : int;
			mutable browse : 'a KDTrees.tree;
			mutable data : ('a point, ('a,'b,'c) info) Hashtbl.t;
		};;


		(* Create a graph from points *)
		let create_graph points init_function multiplicity =
			(* Create the empty hashtbl *)
			let length = Array.length points in
			let ratio = 4 in
			let hashtbl = Hashtbl.create (length*ratio) in
			let create_list x = [] in
			let create_empty_nei () = Array.init multiplicity create_list in
			for i = 0 to (length-1) do
				Hashtbl.add hashtbl points.(i) {info = (init_function ());
												neigh = (create_empty_nei ());}
			done;
			{
				dim = (Array.length points.(0));
				multiplicity = multiplicity;
				browse = (KDTrees.constructKDT points);
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
				| h::t -> if h = y then t else h::(rmv_list t) in
			let cur_value = (Hashtbl.find graph.data x) in
			cur_value.neigh.(m) <- rmv_list cur_value.neigh.(m);
			Hashtbl.replace graph.data x cur_value;;

		(* Add a point *)
		let add_pt graph x init_function =
			(* Add to the KDTree *)
			graph.browse <- KDTrees.addTree x graph.browse graph.dim;
			let create_list x = [] in
			let create_empty_nei () = Array.init graph.multiplicity create_list in
			(* Add to the hashtbl *)
			Hashtbl.add graph.data x {info = (init_function ());
					neigh = (create_empty_nei ());};;

		(* Remove a point *)
		let rmv_pt graph x =
			(* Add to the KDTree *)
			graph.browse <- KDTrees.removeTree x graph.browse graph.dim;
			(* Add to the hashtbl *)
			Hashtbl.remove graph.data x;;

		(* Get point info *)
		let get_pt graph x = (Hashtbl.find graph.data x).info;;

		(* Set point info *)
		let set_pt graph x info =
			let cur_value = (Hashtbl.find graph.data x) in
			cur_value.info <- info;
			Hashtbl.replace graph.data x cur_value;;

		(* Find the current cell *)
		let find_cell graph (x : 'a point) distance_tools =
			let (_,nearest) = KDTrees.nns graph.browse x distance_tools graph.dim in
			(nearest : 'a point);;

		(* Rebalance the search graph *)
		let rebalance graph = graph.browse <- (KDTrees.rebalance graph.browse);;

		(* Integer pairing *)
		(* Cantor pairing *)
		let cantor_pairing (point : int point) =
			((point.(0) + point.(1)) * (point.(0) + point.(1) + 1) / 2) + point.(1);;
		let cantor_reverse z =
			let w = int_of_float (floor ((sqrt((float_of_int z) *. 8. +. 1.) -. 1.) /. 2.)) in
			let t = (w*w + w) / 2 in
			let y = z - t in
			let x = w - y in
			([|x;y|] : int point);;
		(* Hopcroft/Ullman pairing *)
		let hopcfroft_delta x = x * (x + 1) / 2;;
		let hopcfroft_pairing (point : int point) =
			(hopcfroft_delta (point.(0) + point.(1) - 2)) + point.(0);;
		let hopcfroft_reverse h =
			let c = int_of_float (floor (sqrt((float_of_int (h*2))) -. 0.5)) in
			let i = h - (hopcfroft_delta c) in
			let j = c - i + 2 in
			([|i;j|] : int point);;
		(* Pigeon - Bitwise paring with bits interleaving *)
		let bitwise_pairing (point : int point) =
			let x = ref point.(0) in
			let y = ref point.(1) in
			let p = ref 0 in
			let i = ref 0 in
			while ((!x <> 0) || (!y <> 0)) do
				p := !p lor ((!x land 1) lsl !i);
				x := !x lsr 1;
				p := !p lor ((!y land 1) lsl (!i+1));
				y := !y lsr 1;
				i := !i + 2;
			done;!p;;
		let bitwise_reverse h =
			let x = ref 0 in
			let y = ref 0 in
			let p = ref h in
			let i = ref 0 in
			while (!p <> 0) do
				x := !x lor ((!p land 1) lsl !i);
				p := !p lsr 1;
				y := !y lor ((!p land 1) lsl !i);
				p := !p lsr 1;
				i := !i + 1;
			done;
			([|!x;!y|] : int point);;
		(* Tail recursive int fast exponentiation *)
		let pow base exponent =
			let rec aux accumulator base = function
				| 0 -> accumulator
				| 1 -> base * accumulator
				| e when ((e mod 2) = 0) -> aux accumulator (base * base) (e / 2)
				| e -> aux (base * accumulator) (base * base) ((e - 1) / 2) in
			aux 1 base exponent;;
		(* Godel Numbering *)
		let godel_paring (point : int point) =
				(pow 2 point.(0)) * (pow 2 point.(1));;
		let godel_reverse h =
			(* variables *)
			let x = ref 0 in
			let z = ref 0 in
			(* Galloping *)
			let lo_y = ref 0 in
			let hi_y = ref 0 in
			while (!z mod (pow 3 !hi_y)) = 0 do
				lo_y := !hi_y;
				hi_y := !hi_y * 2;
			done;
			(* Search *)
			while (!lo_y < !hi_y) do
				let test_y = (!hi_y + !lo_y + 1) / 2 in
				if (!z mod (pow 3 test_y)) = 1 then hi_y := test_y - 1
				else lo_y := test_y;
			done;
			(* Result *)
			z := !z / (pow 3 !lo_y);
			x := int_of_float ((log ((float_of_int !z)+.0.01)) /. (log 2.)); (* Numerical stability issue *)
			([|!x;!lo_y|] : int point);;
		(* Pairing for any dimension *)
		let rec global_pairing funct point size = match size with
			| 0 -> failwith "global_pairing: Size = 0"
			| 1 -> failwith "global_pairing: Size = 1"
			| 2 -> funct point
			| _ ->
				(if (size mod 2) = 0 then
					let s2 = (size/2) in
					if s2 = 1 then failwith "HERE";
					funct [|global_pairing funct (Array.sub point 0 s2) s2;
						global_pairing funct (Array.sub point s2 s2) s2|]
					else funct [|point.(0);
						(global_pairing funct (Array.sub point 1 (size-1)) (size-1))|];);;
		(* Reversing for any dimension *)
		let rec global_reverse funct z size = match size with
			| 0 -> failwith "global_reverse: Size = 0"
			| 1 -> failwith "global_reverse: Size = 1"
			| 2 -> funct z
			| _ ->
				if (size mod 2) = 0 then
					let cur = funct z in
					let s2 = (size/2) in
					Array.concat [(global_reverse funct cur.(0) s2);
						(global_reverse funct cur.(1) s2)]
				else
					let cur = funct z in
					Array.concat [[|cur.(0)|];(global_reverse funct cur.(1) ((size-1)))];;

		(* Breadth First Search Algorithm *)
		let bfs g s m =
			(* WARNING: The pairing functions of overflowing easily if the
									dimension is higher than 5 *)
			if (g.dim > 5) then failwith "dijkstra: Doesn't work with too high dimensions";
			(* Get pairing of a point *)
			let h x = global_pairing cantor_pairing x g.dim in
			(* Structure of the header :
					0 -> color
					1 -> father
					2 -> dist
			*)
			let get_col u = (get_pt g u).(0) in
			let get_dist u = (get_pt g u).(2) in
			let set_var u i v =
				let info = (get_pt g u) in
				info.(i)<-v; in
			let set_col u c = set_var u 0 c in
			let set_father u v = set_var u 1 (h v) in
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
			(* WARNING: The pairing functions of overflowing easily if the
									dimension is higher than 5 *)
			if (g.dim > 5) then failwith "dijkstra: Doesn't work with too high dimensions";
			(* Get pairing of a point *)
			let h x = global_pairing cantor_pairing x g.dim in
			let h_r x = global_reverse cantor_reverse x g.dim in
			(* Structure of the header :
					0 -> color
					1 -> father
					2 -> dist
			*)
			let get_col u = (get_pt g u).(0) in
			let get_dist u = (get_pt g u).(2) in
			let set_var u i v =
				let info = (get_pt g u) in
				info.(i)<-v; in
			let set_col u c = set_var u 0 c in
			let set_father u v = set_var u 1 (h v) in
			let set_dist u v = set_var u 2 v in
			(* Variables *)
			let color_ref = Random.bits () in (* Select a random color *)
			if (get_col r) = color_ref then failwith "dijkstra: Wrong color";
			let n = Hashtbl.length g.data in
			let f = PriorityQueue.create n 0 in
			(* Initialisation *)
			set_dist r 0;
			PriorityQueue.push ((h r), 0) f;
			(* Work on neighbors *)
			let rec visit_neigh u l = match l with
				| [] -> ()
				| (v, d) :: l' ->
					if ((get_col v) <> color_ref) && 
							((get_dist v) > (get_dist u) + d || (get_dist v) = -1) then (
						set_father v u;
						set_dist v ((get_dist u) + d);
						if not (PriorityQueue.is_in (h v) f) then
							PriorityQueue.push ((h v), (get_dist v)) f
						else
							PriorityQueue.decrease_prio ((h v), (get_dist v)) f;
					);
					visit_neigh u l' in
			while not (PriorityQueue.is_empty f) do
				let (u,_) = PriorityQueue.pop f in
				let co_u = h_r u in
				set_col co_u color_ref;
				visit_neigh co_u (nei g co_u m)
			done;;

		(* Compute path between two points given a father array *)
		let compute_path g u v =
			(* Get pairing of a point *)
			let h_r x = global_reverse cantor_reverse x g.dim in
			(* Actual computation *)
			let rec aux v pth =
				if v = u then u::pth
				else if (get_pt g v).(1) = (-1) then [] (* failwith "compute_path" *)
				else
					let w = (get_pt g v).(1) in aux (h_r w) (v :: pth) in
			aux v [];;

		(* Shortest path between u and v in g for m using bfs *)
		let shortest_path_bfs g u v m =
			bfs g u m;
			(compute_path g u v, (get_pt g v).(2));;

		(* Shortest path between u and v in g for m using dijkstra *)
		let shortest_path_dijkstra g u v m =
			dijkstra g u m;
			(compute_path g u v, (get_pt g v).(2));;

	end
