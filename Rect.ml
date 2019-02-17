(* Graphic Library *)
#load "graphics.cma";;

module Rect :
	sig
		type 'a rect
		val get_dim : 'a rect -> int
		val create : 'a array -> 'a array -> 'a rect
		val empty : int -> float rect
		val reset : int -> float rect
		val validate : 'a rect -> bool
		val isPoint : 'a rect -> bool
		val are_equal : 'a rect -> 'a rect -> bool
		val center : float rect -> float array
		val distanceFromCenter : float rect -> float rect -> float
		val dimensionOfMinWidth : float rect -> int
		val dimensionOfMaxWidth : float rect -> int
		val setMinCorner : 'a rect -> 'a array -> unit
		val setMaxCorner : 'a rect -> 'a array -> unit
		val getDimension : 'a rect -> int
		val getMinCorner : 'a rect -> 'a array
		val getMaxCorner : 'a rect -> 'a array
		val copyRect : 'a rect -> 'a rect
		val closestPoint : 'a rect -> 'a array -> 'a array
		val contains : 'a rect -> 'a array -> bool
		val contains_line : float rect -> float array -> float array -> bool
		val intersection : 'a rect -> 'a rect -> 'a rect
		val intersectMany : 'a rect list -> 'a rect
		val intersects : 'a rect -> 'a rect -> bool
		val overlap : float rect -> float rect -> float
		val stretch : 'a rect -> 'a rect -> unit
		val volume : float rect -> float
		val perimeter : float rect -> float
		val union : 'a rect -> 'a rect -> 'a rect
		val unionMany : 'a rect list -> 'a rect
		val split : float rect -> int -> float rect * float rect
		val draw : float rect -> unit
		val uniform_points : int -> float rect -> float array list
		val liang_barsky_clipper : float array -> float array -> float rect -> float array * float array
	end =
	struct
		(* Structures *)
		type 'a rect = {
			dim : int;
			mutable minCorner : 'a array;
			mutable maxCorner : 'a array;
		};;

		(* Get the dimension of a rectangle *)
		let get_dim rect = rect.dim;;

		(* Create a rectangle from points *)
		let create a b = {dim = (Array.length a) ; minCorner = a ; maxCorner = b};;

		(* Create an empty rectangle *)
		let empty size = create (Array.make size zero) (Array.make size zero);;

		(* Create an empty rectangle *)
		let reset size = create (Array.make size biggest) (Array.make size lowest);;

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

		(* Are they equal ? *)
		let are_equal rect_a rect_b =
			let i = ref 0 in
			while (!i < rect_a.dim) &&
				(rect_a.minCorner.(!i) = rect_b.minCorner.(!i)) &&
				(rect_a.maxCorner.(!i) = rect_b.maxCorner.(!i)) do
				i := !i + 1;
			done;(!i >= rect_a.dim);;

		(* Center of a rectangle *)
		let center rect =
			let ret = Array.copy rect.minCorner in
			for i = 0 to (rect.dim-1) do
				ret.(i) <- div (add rect.minCorner.(i) rect.maxCorner.(i)) two;
			done;ret;;

		(* Sums the total distances from the center of another rectangle *)
		let distanceFromCenter rect_a rect_b =
			let ret = ref zero in
			for i = 0 to (rect_a.dim-1) do
				let t = div (add (add rect_a.minCorner.(i) rect_a.maxCorner.(i)) (add rect_b.minCorner.(i) rect_b.maxCorner.(i))) two in
				ret := add !ret (mul t t);
			done;!ret;;

		(* Find the index of the dimension having the smallest difference between
			the minimum vertex and maximum vertex point *)
		let dimensionOfMinWidth rect =
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
		let dimensionOfMaxWidth rect =
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
		let getDimension rect = rect.dim;;
		let getMinCorner rect = rect.minCorner;;
		let getMaxCorner rect = rect.maxCorner;;

		(* Copy a rectangle *)
		let copyRect rect =
			{dim = rect.dim;
			minCorner = (Array.copy rect.minCorner);
			maxCorner = (Array.copy rect.maxCorner);};;

		(* Find the closest point on the surface or within 
			the hyper-rectangle to the specified point *)
		let closestPoint rect p =
			let ret = ref (Array.make rect.dim p.(0)) in
			for i = 0 to (rect.dim-1) do
				let d = p.(i) in
				if (d < rect.minCorner.(i)) then
					!ret.(i) <- rect.minCorner.(i)
				else if (d > rect.maxCorner.(i)) then
					!ret.(i) <- rect.maxCorner.(i)
				else
					!ret.(i) <- d;
			done;!ret;;

		(* Tell if the rectangle contains a point *)
		let contains rect p =
			let i = ref 0 in
			let searching = ref true in
			while (!i < rect.dim) && !searching do
				let d = p.(!i) in
				if (d < rect.minCorner.(!i)) || (d > rect.maxCorner.(!i)) then
					searching := false;
				i := !i + 1;
			done;!searching;;

		(* Tell if the rectangle contains a segment *)
		let contains_line rect point_a point_b =
			(* 2D-ONLY Function *)
			if rect.dim <> 2 then failwith "contains_line: 2D Only";
			let rectMin = rect.minCorner in
			let rectMax = rect.maxCorner in
			let min_x = ref (min point_a.(0) point_b.(0)) in
			let max_x = ref (max point_a.(0) point_b.(0)) in
			if !max_x > rectMax.(0) then max_x := rectMax.(0);
			if !min_x < rectMin.(0) then min_x := rectMin.(0);
			if !min_x > !max_x then false
			else
				(let min_y = ref point_a.(1) in
				let max_y = ref point_b.(1) in
				let dx = (minus point_b.(0) point_a.(0)) in
				if (abs_f dx) > precision then
					(let a = div (minus point_b.(1) point_a.(1)) dx in
					let b = minus point_a.(1) (mul a point_a.(0)) in
					min_y := add (mul a !min_x) b;
					max_y := add (mul a !max_x) b;);
				if (!min_y > !max_y) then
					(let tmp = !max_y in
					max_y := !min_y;
					min_y := tmp);
				if !max_y > rectMax.(1) then max_y := rectMax.(1);
				if !min_y < rectMin.(1) then min_y := rectMin.(1);
				if !min_y > !max_y then false
				else true);;

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

		(* Union of multiple rectangles *)
		let intersectMany rects =
			let rec intersectMany_aux l cur = match l with
				| [] -> cur
				| h::t -> intersectMany_aux t (intersection h cur) in
			intersectMany_aux rects (List.hd rects);;

		(* Find if two hyper-rectangles are intersecting *)
		let intersects rect_a rect_b =
			let i = ref 0 in
			let intersect = ref true in
			while (!i < rect_a.dim) && !intersect do
				intersect := not ((rect_a.minCorner.(!i) > rect_b.maxCorner.(!i))
									|| (rect_b.minCorner.(!i) > rect_a.maxCorner.(!i)));
				i := !i + 1;
			done;!intersect;;

		(* Overlap *)
		let overlap rect_a rect_b =
			let area = ref one in
			let axis = ref 0 in
			while (is_0 !area) && (!axis < rect_a.dim) do
				let x1 = rect_a.minCorner.(!axis) in
				let x2 = rect_a.maxCorner.(!axis) in
				let y1 = rect_b.minCorner.(!axis) in
				let y2 = rect_b.maxCorner.(!axis) in
				(* Left edge outside left edge *)
				if (x1 < y1) then 
					(* Right edge inside left edge *)
					(if (y1 < x2) then
						(* Right edge outside right edge *)
						(if (y2 < x2) then area := mul !area (minus y2 y1)
						else area := mul !area (minus x2 y1));)
				(* Right edge inside left edge *)
				else if (x1 < y2) then
					(* Right edge outside right edge *)
					(if (x2 < y2) then area := mul !area (minus x2 x1)
					else area := mul !area (minus y2 x1))
				else
					area := zero;
				axis := !axis + 1;
			done;!area;;

		(* Fits another rectangle inside of this rectangle *)
		let stretch rect_a rect_b =
			for i = 0 to (rect_a.dim-1) do
				if rect_a.minCorner.(i) > rect_b.minCorner.(i) then
					rect_a.minCorner.(i) <- rect_b.minCorner.(i);
				if rect_a.maxCorner.(i) < rect_b.maxCorner.(i) then
					rect_a.maxCorner.(i) <- rect_b.maxCorner.(i);
			done;;

		(* Find the volume of an hyper-rectangle *)
		let volume rect =
			let ret = ref (minus rect.maxCorner.(0) rect.minCorner.(0)) in
			for i = 1 to (rect.dim-1) do
				ret := mul !ret (minus rect.maxCorner.(i) rect.minCorner.(i));
			done;!ret;;

		(* Find the perimeter of an hyper-rectangle *)
		let perimeter rect =
			let ret = ref (minus rect.maxCorner.(0) rect.minCorner.(0)) in
			for i = 1 to (rect.dim-1) do
				ret := add !ret (minus rect.maxCorner.(i) rect.minCorner.(i));
			done;!ret;;

		(* Union of two rectangles *)
		let union rect_a rect_b =
			let rect_ret = copyRect rect_a in
			for i = 0 to (rect_a.dim-1) do
				rect_ret.minCorner.(i) <- min rect_a.minCorner.(i) rect_b.minCorner.(i);
				rect_ret.maxCorner.(i) <- max rect_a.maxCorner.(i) rect_b.maxCorner.(i);
			done;rect_ret;;

		(* Union of multiple rectangles *)
		let unionMany rects =
			let rec unionMany_aux l cur = match l with
				| [] -> cur
				| h::t -> unionMany_aux t (union h cur) in
			unionMany_aux rects (List.hd rects);;

		(* Split a rectangle in half in a specific axis *)
		let split rect m =
			let left_rect = copyRect rect in
			let right_rect = copyRect rect in
			left_rect.maxCorner.(m) <- div rect.maxCorner.(m) two;
			right_rect.minCorner.(m) <- div rect.maxCorner.(m) two;
			(left_rect,right_rect);;

		(* Draw a 2D rectangle *)
		let draw rect =
			if rect.dim <> 2 then failwith "draw: 2D Only";
			let polygon = [|(0,0);(0,0);(0,0);(0,0)|] in
			polygon.(0) <- (to_int rect.minCorner.(0),to_int rect.minCorner.(1));
			polygon.(1) <- (to_int rect.maxCorner.(0),to_int rect.minCorner.(1));
			polygon.(2) <- (to_int rect.maxCorner.(0),to_int rect.maxCorner.(1));
			polygon.(3) <- (to_int rect.minCorner.(0),to_int rect.maxCorner.(1));
			Graphics.draw_poly polygon;;

		(* Uniform random points in a rectangle *)
		let uniform_points nb rect =
			let random_arr () =
				let ret = Array.make rect.dim rect.minCorner.(0) in
				(* Generate a random value *)
				let random_val i =
					let ret = ref (random_f (minus (minus rect.maxCorner.(i) rect.minCorner.(i)) one)) in
					while is_0 !ret do
						ret := (random_f (minus (minus rect.maxCorner.(i) rect.minCorner.(i)) one));
					done;(add !ret rect.minCorner.(i)) in
				for i = 0 to (rect.dim-1) do
					ret.(i)<-(random_val i);
				done;ret in
			let rec build_ret i =
				if i < 0 then []
				else (random_arr ())::(build_ret (i-1)) in
			build_ret nb;;

		(* Liang-Barsky Line-Clipping Algorithm *)
		let liang_barsky_clipper point_a point_b rect =
			(* This is 2D only algorithm *)
			if (Array.length point_a) <> 2 then failwith "liang_barsky_clipper: 2D Only!";
			(* Notations *)
			let xmin = rect.minCorner.(0)
			and xmax = rect.maxCorner.(0)
			and ymin = rect.minCorner.(1)
			and ymax = rect.maxCorner.(1)
			and x1 = point_a.(0)
			and y1 = point_a.(1)
			and x2 = point_b.(0)
			and y2 = point_b.(1) in
			(* Move variables *)
			let p1 = mul m_one (minus x2 x1) in
			let p2 = mul m_one p1 in
			let p3 = mul m_one (minus y2 y1) in
			let p4 = mul m_one p3 in
			let q1 = minus x1 xmin in
			let q2 = minus xmax x1 in
			let q3 = minus y1 ymin in
			let q4 = minus ymax y1 in
			let posarr = Array.make 5 zero in
			posarr.(0)<-one;
			let negarr = Array.make 5 zero in
			negarr.(0)<-zero;
			let posind = ref 1
			and negind = ref 1 in
			(* Smart increment *)
			let smart_incr x = (x := !x + 1);(!x-1) in
			(* If segment is parallel to the rectangle sides *)
			if (((is_0 p1) && q1 < precision) || ((is_0 p3) && q3 < precision)) then
				([||],[||])
			else
				begin
				if not (is_0 p1) then
					(let r1 = div q1 p1 in
					let r2 = div q2 p2 in
					if (p1 < precision) then
						(negarr.((smart_incr negind))<-r1; (* For negative p1, add it to negative array *)
						posarr.((smart_incr posind))<-r2) (* and add p2 to positive array *)
					else
						(negarr.((smart_incr negind))<-r2;
						posarr.((smart_incr posind))<-r1););
				if not (is_0 p3) then
					(let r3 = div q3 p3 in
					let r4 = div q4 p4 in
					if (p3 < precision) then
						(negarr.((smart_incr negind))<-r3;
						posarr.((smart_incr posind))<-r4)
					else
						(negarr.((smart_incr negind))<-r4;
						posarr.((smart_incr posind))<-r3););
				(* Maximum of negative array / Minimum of positive array *)
				let maxi arr n =
					let m = ref zero in
					for i = 0 to (n-1) do
						if (!m < arr.(i)) then m := arr.(i);
					done;!m in
				let mini arr n =
					let m = ref one in
					for i = 0 to (n-1) do
						if (!m > arr.(i)) then m := arr.(i);
					done;!m in
				let rn1 = maxi negarr !negind in
				let rn2 = mini posarr !posind in
				(* If line is outside the rectangle *)
				if (rn1 > rn2) then
					([||],[||])
				else
					([|(add x1 (mul p2 rn1));(add y1 (mul p4 rn1))|],
					 [|(add x1 (mul p2 rn2));(add y1 (mul p4 rn2))|]);
				end;;
	end
