(* Graph Tools *)
type couleur = White | Black
type state = int
type weight = int
type graph = (state * weight) list array

let rec visit_neighboor u l prevs dist colors f =
	match l with
	| [] -> ()
	| (v, d) :: l' ->
		if colors.(v) = White && 
				(dist.(v) > dist.(u) + d || dist.(v) = -1) then (
			prevs.(v) <- u;
			dist.(v) <- dist.(u) + d;
			if not (PriorityQueue.is_in v f) then
				PriorityQueue.push (v, dist.(v)) f
			else
				PriorityQueue.decrease_prio (v, dist.(v)) f
		);
		visit_neighboor u l' prevs dist colors f 

let dijkstra g r = 
	let n = Array.length g in
	let prevs = Array.make n (-1)
	and dist = Array.make n (-1)
	and colors = Array.make n White
	and f = PriorityQueue.create n in
	prevs.(r) <- -1;
	dist.(r) <- 0;
	PriorityQueue.push (r, 0) f;
	while not (PriorityQueue.is_empty f) do
		let (u, p) = PriorityQueue.pop f in
		colors.(u) <- Black;
		visit_neighboor u g.(u) prevs dist colors f
	done;
	(prevs, dist)

let rec path prevs u r =
    if u = r then [r]
    else if prevs.(u) = -1 then failwith "path"
    else u::(path prevs (prevs.(u)) r);;

(* Create a grid *)
let create_grid (h,w) nb_line nb_row =
	let graph = Array.make ((nb_line*nb_row)+1) [] in
	let pos_from_coor i j = 
		if i = 1 then j
		else (((i-1)*nb_row)+j) in
	let up i j = ((pos_from_coor (i-1) j),1) in
	let down i j = ((pos_from_coor (i+1) j),1) in
	let right i j = ((pos_from_coor i (j+1)),1) in
	let left i j = ((pos_from_coor i (j-1)),1) in
	(* Borders *)
	for j = 2 to (nb_row-1) do
		graph.(pos_from_coor 1 j)<-[(left 1 j);(down 1 j);(right 1 j)];
		graph.(pos_from_coor nb_line j)<-[(up nb_line j);
																					(left nb_line j);(right nb_line j)];
	done;
	for i = 2 to (nb_line-1) do
		graph.(pos_from_coor i 1)<-[(up i 1);(down i 1);(right i 1)];
		graph.(pos_from_coor i nb_row)<-[(up i nb_row);(down i nb_row);
																				 (left i nb_row)];
	done;
	(* Corners *)
	graph.(pos_from_coor 1 1) <- [(right 1 1);(down 1 1)];
	graph.(pos_from_coor 1 nb_row) <- [(left 1 nb_row);(down 1 nb_row)];
	graph.(pos_from_coor nb_line 1) <- [(up nb_line 1);(right nb_line 1)];
	graph.(pos_from_coor nb_line nb_row) <- [(left nb_line nb_row);(up nb_line nb_row)];
	(* Rest *)
	for i = 2 to (nb_line-1) do
		for j = 2 to (nb_row-1) do
			graph.(pos_from_coor i j)<-[(up i j);(down i j);(left i j);(right i j)];
		done;
	done;
	graph;;
