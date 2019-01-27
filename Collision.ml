module Collision :
	sig
		type 'a segment = 'a array array
		type 'a line = 'a segment list
		type 'a polygon = 'a segment list
		type 'a points = 'a KDTrees.tree
		type 'a border =
			| Line of 'a line
			| Polygon of 'a polygon
			| Points of 'a points
		type 'a scene = ('a, 'a border) RTree.tree
		val image_to_scene : string -> int -> int scene
		val draw_scene : int scene -> unit
		val check_collision : int scene -> int array -> int array -> bool
	end =
	struct
		(* Various types *)
		type 'a segment = 'a array array;;
		type 'a line = 'a segment list;;
		type 'a polygon = 'a segment list;;
		type 'a points = 'a KDTrees.tree;;
		type 'a border =
			| Line of 'a line
			| Polygon of 'a polygon
			| Points of 'a points;;

		(* Scene *)
		type 'a scene = ('a, 'a border) RTree.tree;;

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

		(* WARNING - 2D ONLY FOR NOW *)

		(* Cross line  - Taken from https://bryceboe.com/2006/10/23/line-segment-intersection-algorithm/ *)
		let ccw a b c =
				(mul (minus c.(1) a.(1)) (minus b.(0) a.(0))) > (mul (minus b.(1) a.(1)) (minus c.(0) a.(0)));;
		let cross_segment_aux a b c d =
			 ((ccw a c d) <> (ccw b c d)) && ((ccw a b c) <> (ccw a b d));;
		let cross_segment seg_a seg_b = cross_segment_aux seg_a.(0) seg_a.(1) seg_b.(0) seg_b.(1);;

		(* Cross line *)
		let rec cross_line point_a point_b l = match l with
			| seg::t ->
				if (cross_segment seg [|point_a;point_b|]) then true
				else cross_line point_a point_b t
			| [] -> false;;

		(* Cross polygon *)
		let cross_polygon point_a point_b poly = cross_line point_a point_b poly;;

		(* Cross border *)
		let cross_border point_a point_b border = match border with
			| Line a -> cross_line point_a point_b a
			| Polygon a -> cross_polygon point_a point_b a
			| Points a -> KDTrees.intersect_segment point_a point_b 2 a;;

		(* Draw a border *)
		let draw_segement seg =
			Graphics.moveto seg.(0).(0) seg.(0).(1);
			Graphics.lineto seg.(1).(0) seg.(1).(1);;
		let rec draw_line line = match line with
			| seg::t -> draw_segement seg; draw_line t;
			| [] -> ();;
		let draw_polygon poly = draw_line poly;;
		let draw_points points = KDTrees.drawTree points;;
		let draw_border border = match border with
			| Line a -> draw_line a
			| Polygon a -> draw_polygon a
			| Points a -> draw_points a;;

		(* Image operations *)
		(* Get height and width of an image *)
		let getHeight img = Array.length img;;
		let getWidth img = Array.length img.(0);;
		let getHW m = (getHeight m,getWidth m);;
		(* Import an image *)
		let import_image file = Image_magick.lire_image file;;
		(* Apply a function to each element of a matrix *)
		let matrixApply f matrix =
			let (h,w) = getHW matrix in
			let ret = Array.make_matrix h w (f matrix.(0).(0)) in
			for i = 0 to (h- 1) do
				for j = 0 to (w - 1) do
					ret.(i).(j) <- f (matrix.(i).(j))
				done;
			done;ret;;
		(* Convert RGB integer to color type *)
		let color_of_rgbint (num : Graphics.color) =
			(* Red/Green/Blue *)
			let b = num mod 256 in
			let g = (num/256) mod 256 in
			let r = (num/256/256) mod 256 in
			(r,g,b);;
		(* Convert RGB to greyscale *)
		let greyscale_of_rgb pix = let (r,g,b) = color_of_rgbint pix in
			(float_of_int (r + g + b))/.3.;;
		(* Convert whole image to greyscale *)
		let imageToGreyScale image = (matrixApply greyscale_of_rgb image);;

		(* Graham scan - Convex Hull algorithm *)
		(* From https://github.com/tsoding/convex-hull *)
		let graham_scan points =
		  let swap (a,b) = (b,a) in
		  let mapn f (x,y) =
		    let a = f x in
		    (a, f y) in
		  let ( %> ) f g x = g (f x) in
		  let uncurry f (x,y) = f x y in
			let find_start_point ps =
			  let accumulate p1 p2 = swap @@ min (swap p1) (swap p2) in
			  List.fold_left accumulate (max_int, max_int) ps in
			let angle_sort (sx, sy) ps =
			  let vs = List.map (fun (px, py) -> (px - sx, py - sy)) ps in
			  let angles = List.map (mapn float_of_int %> swap %> uncurry atan2) vs
			  in angles
			     |> List.combine ps
			     |> List.sort (fun (_, a1) (_, a2) -> Pervasives.compare a1 a2)
			     |> List.map fst in
			let ccw (x1, y1) (x2, y2) (x3, y3) =
			  let (v1x, v1y) = (x2 - x1, y2 - y1) in
			  let (v2x, v2y) = (x3 - x2, y3 - y2) in
			  v1x * v2y - v1y * v2x in
			let rec drop_until_ccw p1 convex_hull =
			  match convex_hull with
			  | p2 :: p3 :: rest_convex_hull when ccw p1 p2 p3 <= 0 ->
			     drop_until_ccw p1 (p3 :: rest_convex_hull)
			  | rest_convex_hull -> rest_convex_hull in
			let rec graham_scan sorted_ps =
			  match sorted_ps with
			  | p1 :: rest_sorted_ps ->
			     p1 :: (rest_sorted_ps |> graham_scan |> drop_until_ccw p1)
			  | [] -> [] in
		  let start_point = find_start_point points in
		  let sorted_ps = start_point :: (List.filter ((<>) start_point) points
		                                  |> angle_sort start_point) in
		  graham_scan sorted_ps;;

		(* Randomize an array *)
		let randomize_array arr n =
			let swap i j =
				let tmp = arr.(i) in
				arr.(i) <- arr.(j);
				arr.(j) <- tmp in
			for i = 0 to (n/5) do
				swap (Random.int (n-1)) (Random.int (n-1));
			done;;

		(* Build a scene from an image *)
		let matrix_to_points_custom m start_x start_y end_x end_y =
			let points = ref [] in
			let min_x = ref biggest in
			let min_y = ref biggest in
			let max_x = ref lowest in
			let max_y = ref lowest in
			for i = start_x to end_x do
				for j = start_y to end_y do
					if m.(i).(j) < 127. then
						(points := [|i;j|]::!points;
						max_x := max !max_x i;
						min_x := min !min_x i;
						max_y := max !max_y j;
						min_y := min !min_y j)
				done;
			done;
			((Array.of_list !points),[|!min_x;!min_y|],[|!max_x;!max_y|]);;
		let matrix_to_points m start_x start_y res =
			matrix_to_points_custom m start_x start_y (start_x+res) (start_y+res);;
		let points_to_kdtree points =
			let (ret,_) = KDTrees.constructKDT points in ret;;
		let matrix_to_scene m resolution_min =
			(* Make the empty scene *)
			let ret_scene = RTree.empty_tree 2 in
			(* Browse the matrix *)
			let (h,w) = getHW m in
			(* Edit the tree *)
			let tree_edit pts rect_x rect_y =
				let n = (Array.length pts) in
				if n > 1 then
					(randomize_array pts n;
					let kdt = points_to_kdtree pts in
					let rect = (Rect.create rect_x rect_y) in
					let center = (Rect.center rect add div two) in
					RTree.insert ret_scene rect (RTree.tuple_to_leaf_data center (Points kdt))) in
			(* Check if the resolution is adapted *)
			let i = ref 0 in
			while (!i < (h-resolution_min)) do
				let j = ref 0 in
				while (!j < (w-resolution_min)) do
					(* Insert in tree *)
					let (pts,rect_x,rect_y) = matrix_to_points m !i !j resolution_min in
					tree_edit pts rect_x rect_y;
					(* Incerment *)
					j := !j + resolution_min;
				done;
				i := !i + resolution_min;
			done;
			(* Cover dead pixels *)
			let (pts1,rect_x1,rect_y1) =
				matrix_to_points_custom m (h-(h mod resolution_min)) 0 (h-1) (w-1) in
			let (pts2,rect_x2,rect_y2) =
				matrix_to_points_custom m 0 0 (h-(h mod resolution_min)) (w mod resolution_min) in
			tree_edit pts1 rect_x1 rect_y1;
			tree_edit pts2 rect_x2 rect_y2;
			(ret_scene : int scene);;
		let image_to_scene file res =
			let image = import_image file in
			let matrix = imageToGreyScale image in
			(matrix_to_scene matrix res);;

		let matrix_get file res =
			let image = import_image file in
			let matrix = imageToGreyScale image in
			matrix;;

		(* Draw a scene *)
		let draw_scene (sc : int scene) =
			let draw_kdt t = match t with
				| Points t -> (KDTrees.drawTree t)
				| _ -> failwith "draw_scene: Only points for now" in
			RTree.draw_tree sc;
			RTree.apply_tree draw_kdt sc;;

		(* Check if a collision occurs *)
		let check_collision sc point_a point_b =
			let dangers = RTree.find_segment point_a point_b sc in
			let rec check_collision_aux l = match l with
				| h::t ->
					let (_,_,border) = RTree.leaf_to_tuple h in
					if cross_border point_a point_b border then true
					else check_collision_aux t
				| [] -> false in
			check_collision_aux dangers;;
	end