(* Global flags *)
let h_eps = 1.;;
let disable_heuristic = false;;
let keymodifier = ref 0.;;
let lastKmUpdate_x = ref (-1.);;
let lastKmUpdate_y = ref (-1.);;
let infinite_cost = 100000000.;;

(* Special graph structure for Parti-Game *)
type pg_graph = {
	graph : (float, float array) CustomGraph.graph;
	scene : float Collision.scene;
	mutable start : int;
	mutable goal : int;
	mutable goal_x : float;
	mutable goal_y : float;
	mutable robot_x : float;
	mutable robot_y : float;
	queue : int PriorityQueue.t;
};;

(* Modifying state header *)
(* 0 -> Reserved
	 1 -> Reserved
	 2 -> Reserved
	 3 -> g
	 4 -> h
	 5 -> rhs
	 6/7 -> key1,key2
	 8 -> searchtree *)
let get_g graph state = (CustomGraph.get_node graph.graph state).(3);;
let get_h graph state = (CustomGraph.get_node graph.graph state).(4);;
let get_rhs graph state = (CustomGraph.get_node graph.graph state).(5);;
let get_key1 graph state = (CustomGraph.get_node graph.graph state).(6);;
let get_key2 graph state = (CustomGraph.get_node graph.graph state).(7);;
let get_searchtree graph state =
	int_of_float ((CustomGraph.get_node graph.graph state).(8));;
let set_var graph state i value =
	let info = (CustomGraph.get_node graph.graph state) in
	info.(i)<-value;;
let set_g graph state value = (set_var graph state 3 value);;
let set_h graph state value = (set_var graph state 4 value);;
let set_rhs graph state value = (set_var graph state 5 value);;
let set_key1 graph state value = (set_var graph state 6 value);;
let set_key2 graph state value = (set_var graph state 7 value);;
let set_searchtree graph state value =
	(set_var graph state 8 (float_of_int value));;

(* Key structure *)
type key = float * float;;
let get_key graph state = (((get_key1 graph state),(get_key2 graph state)) : key);;
let set_key graph state (x,y) =
	set_key1 graph state x;
	set_key2 graph state y;;

(* Heuristic of a state *)
let stateheuristic graph x y =
	let h_eps = 1. in
	let ret = ref 0. in
	if (x >= 0.) && (y >= 0.) && (graph.goal_x >= 0.) && (graph.goal_y >= 0.) && (not disable_heuristic) then
		ret := h_eps *. (abs_float(x -. graph.goal_x) +. abs_float(y -. graph.goal_y));
	!ret;;

(* Array to tuple *)
let arr_to_tuple arr = (arr.(0),arr.(1));;

(* Distance between two states *)
let get_distance graph state1 state2 =
	let state1_point = CustomGraph.get_point graph.graph state1 in
	let state2_point = CustomGraph.get_point graph.graph state2 in
	max (abs_float (state1_point.(0) -. state2_point.(0)))
			(abs_float (state1_point.(1) -. state2_point.(1)));;

(* Calculate key of a state *)
let calculatekey state graph =
	let (state_x,state_y) = arr_to_tuple (CustomGraph.get_point graph.graph state) in
	let h = (stateheuristic graph state_x state_y) +. !keymodifier in
	((min infinite_cost ((min (get_rhs graph state) (get_g graph state)) +. h)),
		(min (get_rhs graph state) (get_g graph state)) : key);;

(* Priority queue manipulation *)
let order graph x y = (get_key graph x) <= (get_key graph y);;
let top graph = PriorityQueue.first graph.queue;;
let topkey graph = get_key graph (top graph);;
let pop graph =
	let ret = top graph in
	PriorityQueue.remove_first graph.queue;
	ret;;
let update graph s k =
	let cur_prio = get_key graph s in
	set_key graph s k;
	if cur_prio < k then PriorityQueue.reorder_up graph.queue s
	else PriorityQueue.reorder_down graph.queue s;;
let insert graph s k =
	set_key graph s k;
	PriorityQueue.add graph.queue s;;
let remove graph s =
	PriorityQueue.remove graph.queue s;;

(* Reevaluates max equation for a given action *)
let reevaluatemaxequation graph state action =
	(* Choose the worst successor *)
	let rec worst_succ l maximum maxstate = match l with
		|succ::t ->
			let n_cost_plus_g = (get_distance graph succ state) +. (get_g graph succ) in
			if n_cost_plus_g > maximum then worst_succ t n_cost_plus_g succ
			else worst_succ t maximum maxstate
		| [] -> (maximum,maxstate) in
	let (_,maxstate) =
		worst_succ (CustomGraph.get_edge_successors action) min_float (-1) in
	(* Change the successor for the action if necessary *)
	if (CustomGraph.get_edge_maxstate action) <> maxstate then
		(CustomGraph.set_edge_maxstate action maxstate;
		(* If the original cost is not infinite then it also has to be reset *)
		if (CustomGraph.get_edge_cost action) <> infinite_cost then
			CustomGraph.set_edge_cost action (get_distance graph state maxstate););;

let righthandside graph state =
	if state = graph.start then 0.
	else
		(* Minimax evaluation *)
		(let nei = CustomGraph.nei graph.graph state 0 in
		let rec browse_nei l minimum minimum_state = match l with
			| (n,action)::t ->
				(* Reevaluate max equation for the selected action if necessary *)
				if true then reevaluatemaxequation graph state action;
				let cur = ((CustomGraph.get_edge_cost action) +.
					 get_g graph (CustomGraph.get_edge_maxstate action)) in
				if cur < minimum then browse_nei t cur n
				else browse_nei t minimum minimum_state
			| [] -> (minimum,minimum_state) in
		let (minimum,minimum_state) = browse_nei nei infinite_cost (-1) in
		set_searchtree graph state minimum_state;
		minimum);;

(* Insertition conditions *)
let insertifinconsistent graph state =
	let new_rhs = righthandside graph state in
	set_rhs graph state new_rhs;
	if new_rhs <> (get_g graph state) then
		insert graph state (calculatekey state graph);;
let insertifinconsistentgivenrhs graph state rhs_value =
	if (rhs_value <> (get_rhs graph state)) then
		insert graph state (calculatekey state graph);;

(* Initialize everything *)
let dstarsetup graph =
	(* Initialize the queue *)
	insertifinconsistent graph graph.start;
	let insert_predecessors l = match l with
		| (pred,_)::t -> insertifinconsistent graph pred
		| [] -> () in
	insert_predecessors (CustomGraph.nei graph.graph graph.start 1);;

(* Returns true if all the successors of the robot state have been expanded and are
	 finite whereas robot state is infinite.
	 This is in fact a condition for the robot state to be disconnected from other states *)
let isRobotStateCut graph =
	let kmin = get_key graph (top graph) in
	let stop = ref false in
	let rec iteratesuccessors l stop = match l with
		| succ::t ->
			if !stop then ()
			else
				(* Compute the "kmin" key for this successor *)
				let (state_x,state_y) = arr_to_tuple (CustomGraph.get_point graph.graph succ) in
				let h = (stateheuristic graph state_x state_y) +. !keymodifier in
				let key_0 = min infinite_cost ((min (get_h graph succ) (get_rhs graph succ))+.h) in
				let key_1 = min (get_g graph succ) (get_rhs graph succ) in
				if ((key_0,key_1) > kmin) || ((get_g graph succ) <> (get_rhs graph succ))
																	|| ((get_g graph succ) = infinite_cost) then
					stop := false;
				iteratesuccessors t stop
		| [] -> () in
	let rec iterateactions l = match l with
		| (_,action)::t ->
				if !stop then false
				else
					(* This is just an optimization. The condition below makes sure that
						 for each action cost = infinity *)
					(if (CustomGraph.get_edge_cost action) < infinite_cost then
						stop := false
					else
						iteratesuccessors (CustomGraph.get_edge_successors action) stop;
					iterateactions t)
		| [] -> true in
	(* Optimize the order in which this is done *)
	if (get_rhs graph graph.goal) <> infinite_cost || (get_g graph graph.goal) <> infinite_cost then false
	else iterateactions (CustomGraph.nei graph.graph graph.goal 0);;

let dstarstep graph =
	let robot_state = graph.goal in
	while ((get_key graph (top graph)) < (calculatekey robot_state graph))
				|| ((get_rhs graph robot_state) > (get_g graph robot_state))
				&& not (isRobotStateCut graph) do
		let state = (top graph) in
		let rhs = get_rhs graph state in
		(* Iterate over predecessor *)
		let rec iter1_predecessors l = match l with
			| (pred,_)::t ->
				if (get_rhs graph pred) >=
					min infinite_cost ((get_g graph state) +. (get_distance graph pred state)) then
					insertifinconsistent graph pred
				else
					insertifinconsistentgivenrhs graph pred (get_rhs graph pred);
				iter1_predecessors t
			| [] -> () in
		let rec iter2_predecessors g_old l = match l with
			| (pred,_)::t ->
				if (get_rhs graph pred) >=
					min infinite_cost (g_old +. (get_distance graph pred state)) then
					insertifinconsistent graph pred
				else
					insertifinconsistentgivenrhs graph pred (get_rhs graph pred);
				iter2_predecessors g_old t
			| [] -> () in
		(* Over consistent *)
		if rhs < (get_g graph state) then
			((* Delete the state to be expanded *)
			PriorityQueue.remove_first graph.queue;
			(* Update rhs *)
			set_g graph state rhs;
			(* Insert consistantly *)
			iter1_predecessors (CustomGraph.nei graph.graph state 1);)
		(* Under consistent *)
		else if rhs > (get_g graph state) then
			(let g_old = get_g graph state in
			set_g graph state infinite_cost;
			insertifinconsistent graph state;
			(* Insert consistantly *)
			iter2_predecessors g_old (CustomGraph.nei graph.graph state 1);)
		else ();
	done;;

(* Setup the graph *)
let initializegvalues graph =
	let size = CustomGraph.get_count graph.graph in
	(* Set initial g/rhs values *)
	for i = 0 to (size-1) do
		set_g graph i infinite_cost;
		set_rhs graph i infinite_cost;
		set_searchtree graph i (-1);
	done;
	set_rhs graph graph.start 0.;;

(* Setup initial short path methods *)
let partigame_setup graph =
	(* Initialize the graph *)
	initializegvalues graph;
	(* Initialize algorithm for shortest path computations *)
	dstarsetup graph;
	(* Initialize computation of the shortest path *)
	dstarstep graph;;

(* Returns the indices of the next target (successor) state in the passed parms
	 in return code it returns:
	 * CODE_NEXTSTATE_FOUND - next state is found
	 * CODE_INFINITECOST - next state is not found since infinite cost for all states
	 * CODE_ERROR - next state is not found because of some internal error *)
type state_code =
	| CODE_NEXTSTATE_FOUND
	| CODE_INFINITECOST
	| CODE_ERROR;;
let getNextState graph next_x next_y source next =
	let robot_state = (CustomGraph.find_node graph.graph [|graph.robot_x;graph.robot_y|]) in
	let robot_g = get_g graph robot_state in
	if robot_g = infinite_cost then
		(next := (-1);CODE_INFINITECOST)
	else if (get_searchtree graph robot_state) <> (-1) then
		begin
		let rec browse_actions min_action n_min l = match l with
			|(n,atmp)::t ->
				(* In general only important when ties are not broken
					 in favor of smaller g values *)
				reevaluatemaxequation graph robot_state atmp;
				let succstate = CustomGraph.get_edge_maxstate atmp in
				(* If smaller state is found *)
				let new_cost = (CustomGraph.get_edge_cost atmp) +. (get_g graph succstate) in
				if new_cost < n_min then
					(* That is the new running minimum *)
					browse_actions atmp new_cost t
				else if new_cost = n_min then
					(* Break the tie in geometrical order *)
					(let n_coord = CustomGraph.get_point graph.graph n in
					let min_coord =
						CustomGraph.get_point graph.graph (CustomGraph.get_edge_succstate min_action) in
					if (infinite_cost *. n_coord.(0) +. n_coord.(1) <=
							infinite_cost *. min_coord.(0) +. min_coord.(1)) then
						(* That is the new running minimum *)
						browse_actions atmp new_cost t
					else
						browse_actions min_action n_min t)
				else
					browse_actions min_action n_min t
			| [] -> (min_action,n_min) in
		let (min_action,n_min) =
			browse_actions (snd (List.hd (CustomGraph.nei graph.graph robot_state 0)))
				infinite_cost (CustomGraph.nei graph.graph robot_state 0) in
		let min_coord =
			CustomGraph.get_point graph.graph (CustomGraph.get_edge_succstate min_action) in
		next_x := min_coord.(0);
		next_y := min_coord.(1);
		next := CustomGraph.get_edge_succstate min_action;
		CODE_NEXTSTATE_FOUND
		end
	else
		CODE_ERROR;;

(* Add new coordonates to path *)
let addLocalPathMinimax graph maze_iter current_state next_state path_queue =
	(* Get a square of the center of the next state *)
	let next_coord = CustomGraph.get_point graph.graph next_state in
	let current_coord = CustomGraph.get_point graph.graph current_state in
	let current_rect = CustomGraph.find_rect graph.graph current_coord in
	(* Iterate until we exit the current state *)
	while Rect.contains current_rect maze_iter do
		(* Choose the square that minimizes the maxiumum difference *)
		let x_diff = next_coord.(0) -. maze_iter.(0) in
		let y_diff = next_coord.(1) -. maze_iter.(1) in
		let local_dx = ref 0. in
		let local_dy = ref 0. in
		if x_diff <> 0. && y_diff <> 0. then
			(* Move diagonally *)
			(local_dx := if x_diff > 0. then 1. else (-1.);
			local_dy := if y_diff > 0. then 1. else (-1.))
		else if x_diff <> 0. then
			(* Move along X *)
			local_dx := if x_diff > 0. then 1. else (-1.)
		else if y_diff <> 0. then
			(* Move along Y *)
			local_dy := if y_diff > 0. then 1. else (-1.)
		else ();
		maze_iter.(0)<-maze_iter.(0) +. !local_dx;
		maze_iter.(1)<-maze_iter.(1) +. !local_dy;
	done;Queue.push maze_iter path_queue;;

(* Get the next successor state (even though it might differ from maxstate) *)
let getNextMazeSquare graph next =
	if graph.start = graph.goal then [|graph.goal_x;graph.goal_y|]
	else
		let next_x = ref (-1.) in
		let next_y = ref (-1.) in
		let next_state = ref (-1) in
		let next_try = getNextState graph next_x next_y graph.goal next in
		if next_try = CODE_NEXTSTATE_FOUND then [|graph.goal_x;graph.goal_y|]
		else
			(* Add to the current path a path from iterator's square to the next state *)
			(next := !next_state;
			let path_queue = Queue.create () in
			let maze_iter = [|graph.goal_x;graph.goal_y|] in
			addLocalPathMinimax graph maze_iter graph.goal !next_state path_queue;
			let path_length = Queue.length path_queue in
			if path_length = 0 then maze_iter
			else Queue.top path_queue);;

(* Move the robot *)
let move_robot graph coord state =
	graph.robot_x<-coord.(0);
	graph.robot_y<-coord.(1);
	graph.goal_x<-coord.(0);
	graph.goal_y<-coord.(1);
	graph.goal<-state;;

(* Edit/Add actions *)
(* NOTE: Can't be moved to CustomGraph because of equation evaluation call *)
(* Adds a successor state to the list of possible actions for a given action *)
let addstateaction graph source action state =
	(* If it is already there then we are done *)
	if CustomGraph.is_in_successors action state then ()
	else
		(CustomGraph.add_successor action state;
		(* Re-evaluate maxstate *)
		reevaluatemaxequation graph source action);;

(* Adds or modify an action *)
let addaction graph state1 cost state2 =
	let found = ref true in
	(* Make sure that the action does not already exist *)
	let rec browse_actions l = match l with
		| (n,action)::t ->
			(* If the action exists, set its cost and remove successor states *)
			if n = state2 then
				(CustomGraph.set_edge_cost action cost;
				CustomGraph.reset_successors action;
				addstateaction graph state1 action state2;)
			else browse_actions t
		| [] -> found := false in
	browse_actions (CustomGraph.nei graph.graph state1 0);
	(* Add state1 as predecessor of state2 *)
	CustomGraph.add_edg graph.graph state1 state2
		(CustomGraph.create_edge state2 infinite_cost) 1;
	(* Otherwise just create if *)
	if not !found then
		let action = CustomGraph.create_edge state2 cost in
		addstateaction graph state1 action state2;;

(* Update the key modifier *)
let updateKeyModifier graph =
	let (robot_x,robot_y) = (graph.goal_x,graph.goal_y) in
	(* Initial update *)
	if (!lastKmUpdate_x = (-1.) && !lastKmUpdate_y = (-1.)) then
		(lastKmUpdate_x := robot_x;
		lastKmUpdate_y := robot_y);
	keymodifier :=
		!keymodifier +. (stateheuristic graph !lastKmUpdate_x !lastKmUpdate_y);
	lastKmUpdate_x := robot_x;
	lastKmUpdate_y := robot_y;;

(* Reset the key modifier *)
let resetKeyModifier graph =
	keymodifier := 0.;
	lastKmUpdate_x := graph.goal_x;
	lastKmUpdate_y := graph.goal_y;;

(* Split a given state *)
let splitState graph state =
	(* Old actions *)
	let old_nei = CustomGraph.nei graph.graph state 0 in
	(* Split the longest axis and add to the graph *)
	let header_init () = Array.make 16 0. in
	let (state1,state2) = CustomGraph.split graph.graph header_init state in
	let state1_point = CustomGraph.get_point graph.graph state1 in
	let state1_rect = CustomGraph.find_rect graph.graph state1_point in
	let state2_point = CustomGraph.get_point graph.graph state2 in
	let state2_rect = CustomGraph.find_rect graph.graph state2_point in
	(* Re-evaluate all the old incoming and outgoing edges *)
	let rec new_edges l = match l with
		| (n,action)::t ->
			let n_point = CustomGraph.get_point graph.graph n in
			let n_rect = CustomGraph.find_rect graph.graph n_point in
			if Rect.are_adjacent state1_rect n_rect then
				(addaction graph state1 (get_distance graph state1 n) n;
				addaction graph n (get_distance graph state1 n) state1);
			if Rect.are_adjacent state2_rect n_rect then
				(addaction graph state2 (get_distance graph state2 n) n;
				addaction graph n (get_distance graph state2 n) state2);
			new_edges t
		| [] -> () in
	new_edges old_nei;
	(* Add actions between the two states *)
	addaction graph state1 (get_distance graph state1 state2) state2;
	addaction graph state2 (get_distance graph state1 state2) state1;
	(* Eventually change the goal state *)
	if (Rect.contains state1_rect [|graph.robot_x;graph.robot_y|]) then
		graph.goal<-state1;
	if (Rect.contains state2_rect [|graph.robot_x;graph.robot_y|]) then
		graph.goal<-state2;
	(* Remove from the heap *)
	remove graph state;;

(* Returns false if no solution exists even after splitting that
	 is cells are not splittable any further otherwise returns true *)
let splitBorderCells graph =
	let rec split_loosers border l = match l with
		| (n,action)::t ->
			(* See if a bordering state is a winning one but was not split yet
				 (g = 0) it was split *)
			if ((get_rhs graph n) < infinite_cost) && ((get_g graph n) > 0.) then
				(* Split the cell *)
				(splitState graph n;border := true)
			else if (n = graph.start) then
				(* If a bordering state is a goal state then the current state
					 needs to be split *)
				border := true
			else ();
			split_loosers border t
		| [] -> () in
	if isRobotStateCut graph then
		(* If the robot state is cut then it is the only state in the
			 region of the lost states and we can just split it and its
			 neighbors split only the robot state and its neighbors *)
		(let border = ref false in
		split_loosers border (CustomGraph.nei graph.graph graph.goal 0);
		(* If the cell was a bordering one then split it as welll *)
		if !border then splitState graph graph.goal;)
	else
		(* Iterate through all the states and split them *)
		(let size = CustomGraph.get_count graph.graph in
		for state = 0 to (size-1) do
			let border = ref false in
			(* Iterate through the actions if a state is a loosing one *)
			if (get_rhs graph state) = infinite_cost then
				split_loosers border (CustomGraph.nei graph.graph state 0);
			if !border then splitState graph state;
		done);
	(* Reinitialize g-values *)
	initializegvalues graph;
	(* Reset the key modifier *)
	resetKeyModifier graph;
	(* Reset the algorithm for the computation of the shortest path *)
	dstarsetup graph;
	(* Recompute the shortest path *)
	dstarstep graph;
	(* Return depending if robot g-value is infinite after resets *)
	not ((get_rhs graph graph.goal) = infinite_cost);;

(* Returns false if no solution exists to the existing problem
	 Returns true if a robot successfully executes a step toward the
	 possible solution *)
let step_execute graph =
	let newExperience = ref false in
	let next = ref (-1) in
	if graph.start = graph.goal then true
	else
		(let ret = ref true in
		let g = get_g graph graph.start in
		(* If no path is found then split cells *)
		if g = infinite_cost then
			(if not (splitBorderCells graph) then ret := false)
		else if !ret then
			((* Get next maze square that the robot has to go into *)
			let maze_coord = getNextMazeSquare graph next in
			(* Is the next state is not as expected *)
			let rec get_action dest l = match l with
				| (n,action)::t ->
					if n = dest then action
					else get_action dest t
				| [] -> failwith "get_action: Action not found" in
			let exec_action = get_action !next (CustomGraph.nei graph.graph graph.goal 0) in
			let source_state = graph.goal in
			if (!next <> graph.goal) && (CustomGraph.is_in_successors exec_action !next) then
				newExperience := true;
			(* If it is empty then move the robot into it and if it is
				a brand new experience add it to the database *)
					(* Remember the old expected cost plus g of going along this action *)
			let (collide,_) = Collision.check_collision graph.scene maze_coord maze_coord in
			if collide then
				(move_robot graph maze_coord !next;
				if !newExperience then
					(* Add the new experience *)
					(addstateaction graph source_state exec_action !next;
					(* Make sure that the key was updated *)
					updateKeyModifier graph;
					(* Update vertex for search algorithm *)
					insertifinconsistent graph source_state;
					(* Redo the computation *)
					dstarstep graph;));)
		else
			(* Update the graph and recompute shortest path *)
			(addaction graph graph.goal infinite_cost !next;
			(* Make sure that the key was updated *)
			updateKeyModifier graph;
			(* Update vertex for search algorithm *)
			insertifinconsistent graph graph.goal;
			(* Redo the computation *)
			dstarstep graph);!ret);;