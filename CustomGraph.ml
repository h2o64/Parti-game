module CustomGraph :
  sig
		type 'a point
		type ('a, 'b) edge
		type ('a, 'b, 'c) info
		type ('a, 'b, 'c) graph
		val create_graph : 'a point array -> 'b -> int -> ('a, 'c, 'b) graph
		val adj : ('a, 'b, 'c) graph -> 'a point -> 'a point -> int -> bool
		val nei : ('a, 'b, 'c) graph -> 'a point -> int -> ('a, 'b) edge list
		val get_edg : ('a, 'b, 'c) graph -> 'a point -> 'a point -> int -> 'b
		val set_edg : ('a, 'b, 'c) graph -> 'a point -> 'a point -> 'd -> int -> unit
		val add_edg : ('a, 'b, 'c) graph -> 'a point -> 'a point -> 'b -> int -> unit
		val add_bunch_edg : ('a, 'b, 'c) graph -> 'a point -> ('a, 'b) edge list -> int -> unit
		val rmv_edg : ('a, 'b, 'c) graph -> 'a point -> ('a, 'b) edge -> int -> unit
		val add_pt : ('a, 'b, 'c) graph -> 'a point -> 'c -> unit
		val rmv_pt : ('a, 'b, 'c) graph -> 'a point -> unit
		val get_pt : ('a, 'b, 'c) graph -> 'a point -> 'c
		val set_pt : ('a, 'b, 'c) graph -> 'a point -> 'c -> unit
		val find_cell : ('a, 'b, 'c) graph -> 'a point -> 'a KDTrees.distance_tools -> 'a point
		val rebalance : ('a, 'b, 'c) graph -> unit
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
		let create_graph points init_info multiplicity =
			(* Create the empty hashtbl *)
			let length = Array.length points in
			let ratio = 4 in
			let hashtbl = Hashtbl.create (length*ratio) in
			let empty_neighbor = Array.make multiplicity [] in
			for i = 0 to (length-1) do
				Hashtbl.add hashtbl points.(i) {info = init_info ; neigh = empty_neighbor};
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
				| (h,v)::t -> if (h = y) then (y,v)::t else set t in
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
		let add_pt graph x info =
			(* Add to the KDTree *)
			graph.browse <- KDTrees.addTree x graph.browse graph.dim;
			(* Add to the hashtbl *)
			Hashtbl.add graph.data x {info = info ; neigh = (Array.make graph.multiplicity [])};;

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

	end
