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
