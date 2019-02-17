module Collision :
	sig
		type 'a scene
		val image_to_scene : string -> int -> float scene
		val draw_scene : float scene -> unit
		val check_collision : float scene -> float array -> float array -> bool
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
		type 'a scene = (('a, 'a border) RTree.tree * int);;

		(* WARNING - 2D ONLY FOR NOW *)

		(* Cross line  - Taken from https://bryceboe.com/2006/10/23/line-segment-intersection-algorithm/ *)
		let ccw a b c =
				(mul (minus c.(1) a.(1)) (minus b.(0) a.(0))) > (mul (minus b.(1) a.(1)) (minus c.(0) a.(0)));;
		let cross_segment_aux a b c d =
			 ((ccw a c d) <> (ccw b c d)) && ((ccw a b c) <> (ccw a b d));;
		let cross_segment seg_a seg_b = cross_segment_aux seg_a.(0) seg_a.(1) seg_b.(0) seg_b.(1);;

		(* Cross line *)
		let rec cross_line point_a point_b l =
		if (Array.length point_a) <> 2 then failwith "cross_line: 2D Only";
		match l with
			| seg::t ->
				if (cross_segment seg [|point_a;point_b|]) then true
				else cross_line point_a point_b t
			| [] -> false;;

		(* Cross polygon *)
		let cross_polygon point_a point_b poly = cross_line point_a point_b poly;;

		(* Cross border *)
		let cross_border point_a point_b border dim = match border with
			| Line a -> cross_line point_a point_b a
			| Polygon a -> cross_polygon point_a point_b a
			| Points a -> KDTrees.intersect_segment point_a point_b dim a;;

		(* Draw a border *)
		let draw_segment seg =
			if (Array.length seg.(0)) <> 2 then failwith "draw_segement: 2D Only";
			Graphics.moveto (to_int seg.(0).(0)) (to_int seg.(0).(1));
			Graphics.lineto (to_int seg.(1).(0)) (to_int seg.(1).(1));;
		let rec draw_line line = match line with
			| seg::t -> draw_segment seg; draw_line t;
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
			let min_x = ref max_int in
			let min_y = ref max_int in
			let max_x = ref min_int in
			let max_y = ref min_int in
			for i = start_x to end_x do
				for j = start_y to end_y do
					if m.(i).(j) < 127. then
						(points := [|of_int i;of_int j|]::!points;
						max_x := max !max_x i;
						min_x := min !min_x i;
						max_y := max !max_y j;
						min_y := min !min_y j)
				done;
			done;
			((Array.of_list !points),[|of_int !min_x;of_int !min_y|],[|of_int !max_x;of_int !max_y|]);;
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
					let center = (Rect.center rect) in
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
			((ret_scene,2) : float scene);;
		let image_to_scene file res =
			let image = import_image file in
			let matrix = imageToGreyScale image in
			(matrix_to_scene matrix res);;

		let matrix_get file res =
			let image = import_image file in
			let matrix = imageToGreyScale image in
			matrix;;

		(* Draw a scene *)
		let draw_scene ((sc_tr,sc_dim) : float scene) =
			let draw_kdt t = match t with
				| Points t -> (KDTrees.drawTree t)
				| _ -> failwith "draw_scene: Only points for now" in
			RTree.draw_tree sc_tr;
			RTree.apply_tree draw_kdt sc_tr;;

		(* Check if a collision occurs *)
		let check_collision (sc_tr,sc_dim) point_a point_b =
			let dangers = RTree.find_segment point_a point_b sc_tr in
			let rec check_collision_aux l = match l with
				| h::t ->
					let (_,_,border) = RTree.leaf_to_tuple h in
					if cross_border point_a point_b border sc_dim then true
					else check_collision_aux t
				| [] -> false in
			check_collision_aux dangers;;
	end