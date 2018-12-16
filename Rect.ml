module Rect :
	sig
		type 'a rect
		val get_dim : 'a rect -> int
		val create : 'a point -> 'a point -> 'a rect
		val empty : int -> 'a -> 'a rect
		val validate : 'a rect -> bool
		val isPoint : 'a rect -> bool
		val dimensionOfMinWidth : 'a rect -> ('a -> 'a -> 'a) -> int
		val dimensionOfMinWidth : 'a rect -> ('a -> 'a -> 'a) -> int
		val setMinCorner : 'a rect -> 'a point -> unit
		val setMaxCorner : 'a rect -> 'a point -> unit
		val getMinCorner : 'a rect -> 'b -> 'a point
		val getMaxCorner : 'a rect -> 'b -> 'a point
		val copyRect : 'a rect -> 'a rect
		val closestPoint : 'a rect -> 'a point -> 'a array ref
		val contains : 'a rect -> 'a point -> bool
		val intersection : 'a rect -> 'a rect -> 'a rect
		val intersects : 'a rect -> 'a rect -> bool
		val volume : 'a rect -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'a
		val union : 'a rect -> 'a rect -> 'a rect
		val unionMany : 'a rect list -> 'a rect
		val split : 'a rect -> int -> ('a -> 'a -> 'a) -> 'a -> 'a rect * 'a rect
	end =
	struct
		(* Structures *)
		type 'a point = 'a array;;
		type 'a rect = {
			dim : int;
			mutable minCorner : 'a point;
			mutable maxCorner : 'a point;
		};;

		(* Get the dimension of a rectangle *)
		let get_dim rect = rect.dim;;

		(* Create a rectangle from points *)
		let create a b = {dim = (Array.length a) ; minCorner = a ; maxCorner = b};;

		(* Create an empty rectangle *)
		let empty size zero = create (Array.make size zero) (Array.make size zero);;

		(* Validate a rectangle *)
		let validate rect =
			let len_min = Array.length rect.minCorner in
			let len_max = Array.length rect.maxCorner in
			(* Verifiy length *)
			if not ((len_min = rect.dim) && (len_max = rect.dim)) then
				(print_string "validate: Not right dimensions\n";
				false)
			else
				(* Verifiy if it is the actual corner *)
				(let i = ref 0 in
				while (!i < rect.dim) && (rect.minCorner.(!i) <= rect.maxCorner.(!i)) do
					i := !i + 1;
				done;
				if !i < rect.dim then
					(print_string "validate: Not a corner\n";
					false;)
				else
					true;);;

		(* Is this hyper-rectangle a point ? *)
		let isPoint rect = rect.minCorner <> rect.maxCorner;;

		(* Find the index of the dimension having the smallest difference between
			the minimum vertex and maximum vertex point *)
		let dimensionOfMinWidth rect minus =
			let ret = ref 0 in
			if (rect.dim > 0) then
				(let minWidth = ref (minus rect.maxCorner.(0) rect.minCorner.(0)) in
				for d = 1 to (rect.dim-1) do
					let width = minus rect.maxCorner.(d) rect.minCorner.(d) in
					if width < !minWidth then
						(minWidth := width;
						ret := d);
				done;);
			!ret;;

		(* Find the index of the dimension having the largest difference between
			the minimum vertex and maximum vertex point *)
		let dimensionOfMaxWidth rect minus =
			let ret = ref 0 in
			if (rect.dim > 0) then
				(let minWidth = ref (minus rect.maxCorner.(0) rect.minCorner.(0)) in
				for d = 1 to (rect.dim-1) do
					let width = minus rect.maxCorner.(d) rect.minCorner.(d) in
					if width > !minWidth then
						(minWidth := width;
						ret := d);
				done;);
			!ret;;

		(* Set/Get the minimum/maximum corners *)
		let setMinCorner rect a = (rect.minCorner <- a);;
		let setMaxCorner rect a = (rect.maxCorner <- a);;
		let getMinCorner rect a = rect.minCorner;;
		let getMaxCorner rect a = rect.maxCorner;;

		(* Copy a rectangle *)
		let copyRect rect =
			{dim = rect.dim;
			minCorner = (Array.copy rect.minCorner);
			maxCorner = (Array.copy rect.maxCorner);};;

		(* Find the closest point on the surface or within 
			the hyper-rectangle to the specified point *)
		let closestPoint rect (p : 'a point) =
			let ret = ref (Array.make rect.dim p.(0)) in
			for i = 0 to (rect.dim-1) do
				let d = p.(i) in
				if (d < rect.minCorner.(i)) then
					!ret.(i) <- rect.minCorner.(i)
				else if (d > rect.maxCorner.(i)) then
					!ret.(i) <- rect.maxCorner.(i)
				else
					!ret.(i) <- d;
			done;ret;;

		(* Tell if the rectangle contains a point *)
		let contains rect (p : 'a point) =
			let i = ref 0 in
			let searching = ref true in
			while (!i < rect.dim) && !searching do
				let d = p.(!i) in
				if (d < rect.minCorner.(!i)) || (d > rect.maxCorner.(!i)) then
					searching := false;
				i := !i + 1;
			done;!searching;;

		(* Find the intersection of this hyper-rectangle with another *)
		let intersection rect_a rect_b =
			let rect_ret = copyRect rect_a in
			let i = ref 0 in
			let fine = ref true in
			while (!i  < rect_a.dim) && !fine do
				rect_ret.minCorner.(!i) <- max rect_a.minCorner.(!i) rect_b.minCorner.(!i);
				rect_ret.maxCorner.(!i) <- min rect_a.maxCorner.(!i) rect_b.maxCorner.(!i);
				fine := not (rect_ret.minCorner.(!i) >= rect_ret.maxCorner.(!i));
				i := !i + 1;
			done;
			if not !fine then failwith "intersection: Doesn't intersect" else rect_ret;;

		(* Find if two hyper-rectangles are intersecting *)
		let intersects rect_a rect_b =
			let i = ref 0 in
			let intersect = ref true in
			while (!i < rect_a.dim) && !intersect do
				intersect := not ((max rect_a.minCorner.(!i) rect_b.minCorner.(!i))
									>= (min rect_a.maxCorner.(!i) rect_b.maxCorner.(!i)));
			done;!intersect;;

		(* Find the volume of an hyper-rectangle *)
		let volume rect minus mul zero =
			let ret = ref zero in
			for i = 0 to (rect.dim-1) do
				ret := mul !ret (minus rect.minCorner.(i) rect.maxCorner.(i));
			done;!ret;;

		(* Union of two rectangles *)
		let union rect_a rect_b =
			let rect_ret = copyRect rect_a in
			for i = 0 to (rect_a.dim-1) do
				rect_ret.minCorner.(i) <- min rect_a.minCorner.(i) rect_b.minCorner.(i);
				rect_ret.maxCorner.(i) <- max rect_a.maxCorner.(i) rect_b.maxCorner.(i);
			done;
			rect_ret;;

		(* Union of multiple rectangles *)
		let unionMany rects =
			let rec unionMany_aux l cur = match l with
				| [] -> cur
				| h::t -> unionMany_aux t (union h cur) in
			unionMany_aux rects (List.hd rects);;

		(* Split a rectangle in half in a specific axis *)
		let split rect m div two =
			let left_rect = copyRect rect in
			let right_rect = copyRect rect in
			left_rect.maxCorner.(m) <- div rect.maxCorner.(m) two;
			right_rect.minCorner.(m) <- div rect.maxCorner.(m) two;
			(left_rect,right_rect);;
	end