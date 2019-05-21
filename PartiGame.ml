(* Global flags *)
let h_eps = 1.;;
let disable_heuristic = false;;
let keymodifier = ref 0.;;
let lastKmUpdate_x = ref (-1.);;
let lastKmUpdate_y = ref (-1.);;
let infinite_cost = 100000000.;;
let stateChange = ref false;;

(* Special graph structure for Parti-Game *)
type pg_graph = {
	graph : (float, float array) CustomGraph.graph;
	scene : float Collision.scene;
	mutable robotgoalstate : int;
	robot_goal_x : float;
	robot_goal_y : float;
	mutable robotstate : int;
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
let get_key_manual graph state =
	(((CustomGraph.get_node graph state).(6),
		(CustomGraph.get_node graph state).(7)) : key);;
let set_key graph state (x,y) =
	set_key1 graph state x;
	set_key2 graph state y;;

(* DEBUG Tools *)
let print_info graph =
	let size = CustomGraph.get_count graph.graph in
	let x_offset = (-20) in
	let y_offset = 10 in
	(* Print nei *)
	let rec print_nei x y l i = match l with
		| (n,action)::t ->
			(* Draw this nei *)
			Graphics.moveto (x+x_offset) (y-y_offset*i);
			Graphics.draw_string (string_of_int n);
			Graphics.moveto (x+x_offset+y_offset) (y-y_offset*i);
			Graphics.draw_string (string_of_float (CustomGraph.get_edge_cost action));
			(* Next *)
			print_nei x y t (i+1);
		| [] -> () in
	for state = 0 to (size-1) do
		let point = CustomGraph.get_point graph.graph state in
		let (x,y) = (int_of_float point.(0),int_of_float point.(1)) in
		(* Set color based on heap *)
		if PriorityQueue.mem graph.queue state then
			Graphics.set_color Graphics.red
		else
			Graphics.set_color Graphics.blue;
		(* Draw the state number *)
		Graphics.moveto (x+x_offset) y;
		Graphics.draw_string ("state = "^(string_of_int state));
		(* Draw heuristics *)
		Graphics.moveto (x+x_offset) (y-1*y_offset);
		Graphics.draw_string ("h =  "^(string_of_float (get_h graph state)));
		Graphics.moveto (x+x_offset) (y-2*y_offset);
		Graphics.draw_string ("g =  "^(string_of_float (get_g graph state)));
		Graphics.moveto (x+x_offset) (y-3*y_offset);
		Graphics.draw_string ("rhs = "^(string_of_float (get_rhs graph state)));
		(* Draw nei *)
		(* print_nei x y (CustomGraph.nei graph.graph state 0) 4; *)
	done;;

(* Heuristic of a state *)
let stateheuristic graph x y =
	let robot_state_point = CustomGraph.get_point graph.graph graph.robotstate in
	let h_eps = 1. in
	let ret = ref 0. in
	if (x >= 0.) && (y >= 0.) && (robot_state_point.(0) >= 0.) && (robot_state_point.(1) >= 0.) && (not disable_heuristic) then
		ret := h_eps *. (abs_float(x -. robot_state_point.(0)) +. abs_float(y -. robot_state_point.(1)));
	!ret;;

(* Array to tuple *)
let arr_to_tuple arr = (arr.(0),arr.(1));;

(* Distance between two points *)
let get_distance_point state1_point state2_point =
	max (abs_float (state1_point.(0) -. state2_point.(0)))
			(abs_float (state1_point.(1) -. state2_point.(1)));;

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
	(* Remember the state heuristic *)
	set_h graph state h;
	((min infinite_cost ((min (get_rhs graph state) (get_g graph state)) +. h)),
		(min (get_rhs graph state) (get_g graph state)) : key);;

(* Priority queue manipulation *)
let order graph x y = (get_key_manual graph x) <= (get_key_manual graph y);;
let top graph = PriorityQueue.first graph.queue;;
let topkey graph =
	if PriorityQueue.is_empty graph.queue then
		((infinite_cost,infinite_cost) : key)
	else get_key graph (top graph);;
let pop graph =
	let ret = top graph in
	PriorityQueue.remove_first graph.queue;
	ret;;
let update graph s k =
	let cur_prio = get_key graph s in
	set_key graph s k;
	if cur_prio > k then PriorityQueue.reorder_up graph.queue s
	else PriorityQueue.reorder_down graph.queue s;;
let insert graph s k =
	set_key graph s k;
	PriorityQueue.add graph.queue s;;
let insertormodifyheap graph s k =
	if PriorityQueue.mem graph.queue s then
		(if (get_key graph s) <> k then update graph s k)
	else
		insert graph s k;;
let remove graph s =
	PriorityQueue.remove graph.queue s;;

(* Check heap validity *)
let check_heap graph =
	let size = CustomGraph.get_count graph.graph in
	(* Iterate through states *)
	for state = 0 to (size-1) do
		if PriorityQueue.mem graph.queue state then
			let (key0,key1) = get_key graph state in
			let state_point = CustomGraph.get_point graph.graph state in
			if ((min infinite_cost ((min (get_rhs graph state) (get_g graph state)) +.
				(stateheuristic graph state_point.(0) state_point.(1)))) <> key0)
				&& ((min (get_rhs graph state) (get_g graph state)) <> key1) then
				failwith "check_heap: Check heap failed";
	done;;
(* Check graph validity *)
let check_graph graph =
	let size = CustomGraph.get_count graph.graph in
	(* Check successors and predecessors *)
	let rec check_predecessors state rect l = match l with
		| (pred,_)::t ->
			if pred = state then
				(* (let pred_point = CustomGraph.get_point graph.graph pred in
				let pred_rect = CustomGraph.find_rect graph.graph pred_point in
				if not (Rect.are_adjacent rect pred_rect) then
					failwith "check_graph: a predecessor is not adjacent"
				else ()) *) ()
			else check_predecessors state rect t
		| [] -> failwith "check_graph: not the right predecessor" in
	let rec for_all_successors state rect l = match l with
		| succ::t ->
			(* print_string "Checking ";
			print_int succ;
			print_string " predecessors.\n"; *)
			check_predecessors state rect (CustomGraph.nei graph.graph succ 1);
			for_all_successors state rect t
		| [] -> () in 
	let rec check_successors state rect l = match l with
		| (succ,action)::t ->
			let succ_point = CustomGraph.get_point graph.graph succ in
			let succ_rect = CustomGraph.find_rect graph.graph succ_point in
			(* Adjacency test *)
			if (not (Rect.are_adjacent rect succ_rect)) && not (succ = state) then
				failwith "check_graph: a successor is not adjacent";
			(* Predecessor test *)
			for_all_successors state rect (CustomGraph.get_edge_successors action)
		| [] -> () in
	(* Iterate through states *)
	for state = 0 to (size-1) do
		(* State values *)
		let state_g = get_g graph state in
		let state_point = CustomGraph.get_point graph.graph state in
		let state_rect = CustomGraph.find_rect graph.graph state_point in
		let state_h = stateheuristic graph state_point.(0) state_point.(1) in
		(* Check values of states *)
		if (state_g +. state_h) > infinite_cost && (state_g <> infinite_cost) then
			failwith "check_graph: g,h values are too large";
		(* Check successors and predecessors *)
		check_successors state state_rect (CustomGraph.nei graph.graph state 0);
	done;;

(* Check currently building path *)
let check_path graph =
	let state = ref graph.robotstate in
	(* Browse actions *)
	let rec browse_actions min_state min_cost l = match l with
		| (n,atmp)::t ->
			let succstate = CustomGraph.get_edge_maxstate atmp in
			let succstate_point = CustomGraph.get_point graph.graph succstate in
			let min_state_point = CustomGraph.get_point graph.graph min_state in
			(* If a smaller state is found *)
			if (min infinite_cost ((CustomGraph.get_edge_cost atmp)+.(get_g graph succstate)))
				< (min infinite_cost ((get_g graph min_state)+.min_cost)) then
				(* That is the new running minimum *)
				browse_actions succstate (CustomGraph.get_edge_cost atmp) t
			(* If the state of the cost + g is found *)
			else if (min infinite_cost ((CustomGraph.get_edge_cost atmp)+.(get_g graph succstate)))
				= (min infinite_cost ((get_g graph min_state)+.min_cost)) then
				(if ((infinite_cost*.succstate_point.(0))+.succstate_point.(1))
						<= ((infinite_cost*.min_state_point.(0))+.min_state_point.(1)) then
					(* That is the new running minimum *)
					browse_actions succstate (CustomGraph.get_edge_cost atmp) t
				else
					browse_actions min_state min_cost t)
			else browse_actions min_state min_cost t
			| [] -> (min_state,min_cost) in
	if (get_g graph !state) = infinite_cost then ()
	else
		(while (!state <> graph.robotgoalstate) do
			let first_action = snd (List.hd (CustomGraph.nei graph.graph !state 0)) in
			let first_state = CustomGraph.get_edge_maxstate first_action in
			let first_cost = infinite_cost in
			let (min_state,min_cost) = browse_actions first_state first_cost (CustomGraph.nei graph.graph !state 0) in
			(* Validate next state *)
			if (min infinite_cost ((get_g graph min_state)+.min_cost))
				<> (get_g graph !state) then
				failwith "check_path: Failed on g+c = g";
			if ((get_g graph min_state) <> (get_rhs graph min_state)) then
				failwith "check_path: Failed on inconsistent state";
			print_int min_state;
			print_string "\n";
			state := min_state;
		done);;

(* Reevaluates max equation for a given action *)
let reevaluatemaxequation graph state action =
	(* Choose the worst successor *)
	let rec worst_succ l maximum maxstate = match l with
		| succ::t ->
			(* Get direct cost between the two states *)
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
	if state = graph.robotgoalstate then 0.
	else
		(* Minimax evaluation *)
		(let nei = CustomGraph.nei graph.graph state 0 in
		let rec browse_nei l minimum minimum_state = match l with
			| (n,action)::t ->
				(* Reevaluate max equation for the selected action if necessary *)
				reevaluatemaxequation graph state action;
				let cur = ((CustomGraph.get_edge_cost action) +.
					 get_g graph (CustomGraph.get_edge_maxstate action)) in
				if cur < minimum then browse_nei t cur n
				else browse_nei t minimum minimum_state
			| [] -> (minimum,minimum_state) in
		let (minimum,minimum_state) = browse_nei nei infinite_cost (-1) in
		set_searchtree graph state minimum_state;
		minimum);;

(* Redo the heap *)
let redoheap graph =
	(* Skip heap reordering if robot has not moved since the last reordering *)
	if !stateChange then
		(let size = CustomGraph.get_count graph.graph in
		for state = 0 to (size-1) do
			if PriorityQueue.mem graph.queue state then
				update graph state (calculatekey state graph);
		done;
		stateChange := false);;

(* Insertition conditions *)
let insertifinconsistent graph state =
	let new_rhs = righthandside graph state in
	set_rhs graph state new_rhs;
	if new_rhs <> (get_g graph state) then
		insertormodifyheap graph state (calculatekey state graph)
	else (if PriorityQueue.mem graph.queue state then remove graph state);;
let insertifinconsistentgivenrhs graph state rhs_value =
	if (rhs_value <> (get_g graph state)) then
		insertormodifyheap graph state (calculatekey state graph)
	else (if PriorityQueue.mem graph.queue state then remove graph state);;

(* Initialize everything *)
let dstarsetup graph =
	(* Initialize the queue *)
	insertifinconsistent graph graph.robotgoalstate;
	let rec insert_predecessors l = match l with
		| (pred,_)::t -> insertifinconsistent graph pred;insert_predecessors t
		| [] -> () in
	insert_predecessors (CustomGraph.nei graph.graph graph.robotgoalstate 1);;

(* Returns true if all the successors of the robot state have been expanded and are
	 finite whereas robot state is infinite.
	 This is in fact a condition for the robot state to be disconnected from other states *)
let isRobotStateCut graph =
	let kmin = topkey graph in
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
	if (get_rhs graph graph.robotstate) <> infinite_cost ||
		(get_g graph graph.robotstate) <> infinite_cost then false
	else iterateactions (CustomGraph.nei graph.graph graph.robotstate 0);;

let dstarstep graph =
	let robot_state = graph.robotstate in
	while (((topkey graph) < (calculatekey robot_state graph))
			|| ((get_rhs graph robot_state) <> (get_g graph robot_state)))
			(* && not (isRobotStateCut graph) *) do
		let state = (pop graph) in
		(* DEBUG *)
		print_string "Expand : ";
		print_int state;
		print_string "\n";
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
			((* Update rhs *)
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
		else
			failwith "dstarstep: Locally consistent node in the heap";
	done;
	(* Various checks *)
	check_graph graph;
	(* check_path graph; *)
	check_heap graph;;

(* Setup the graph *)
let initializegvalues graph =
	let size = CustomGraph.get_count graph.graph in
	(* Set initial g/rhs values *)
	for i = 0 to (size-1) do
		set_g graph i infinite_cost;
		set_rhs graph i infinite_cost;
		set_searchtree graph i (-1);
	done;
	set_rhs graph graph.robotgoalstate 0.;;

(* Setup initial short path methods *)
let partigame_setup graph =
	(* Initialize the graph *)
	initializegvalues graph;
	(* Initialize algorithm for shortest path computations *)
	dstarsetup graph;
	(* Initialize computation of the shortest path *)
	dstarstep graph;;

(* Add new coordonates to path *)
let addLocalPathMinimax graph maze_iter current_state next_state path_queue =
	(* Get a square of the center of the next state *)
	let next_coord = CustomGraph.get_point graph.graph next_state in
	let current_coord = CustomGraph.get_point graph.graph current_state in
	let current_rect = CustomGraph.find_rect graph.graph current_coord in
	let local_dx = ref 0. in
	let local_dy = ref 0. in
	(* Iterate until we exit the current state *)
	while Rect.contains current_rect maze_iter do
		(* Choose the square that minimizes the maxiumum difference *)
		let x_diff = next_coord.(0) -. maze_iter.(0) in
		let y_diff = next_coord.(1) -. maze_iter.(1) in
		if not (is_0 x_diff) && not (is_0 y_diff) then
			(* Move diagonally *)
			(local_dx := if (is_pos x_diff) then 1. else (-1.);
			local_dy := if (is_pos y_diff) then 1. else (-1.))
		else if not (is_0 x_diff) then
			(* Move along X *)
			local_dx := if (is_pos x_diff) then 1. else (-1.)
		else if not (is_0 y_diff) then
			(* Move along Y *)
			local_dy := if (is_pos y_diff) then 1. else (-1.)
		else failwith "stop";
		maze_iter.(0)<-maze_iter.(0) +. !local_dx;
		maze_iter.(1)<-maze_iter.(1) +. !local_dy;
	done;
	Queue.push maze_iter path_queue;;

(* Move the robot *)
let move_robot graph coord state =
	print_string "Robot moved !\n";
	Graphics.lineto (int_of_float coord.(0)) (int_of_float coord.(1));
	stateChange := true;
	graph.robot_x<-coord.(0);
	graph.robot_y<-coord.(1);
	graph.robotstate<-state;;

(* Returns the indices of the next target (successor) state in the passed parms
	 in return code it returns:
	 * CODE_NEXTSTATE_FOUND - Next state is found
	 * CODE_INFINITECOST - Next state is not found since infinite cost for all states
	 * CODE_ERROR - Next state is not found because of some internal error *)
type state_code =
	| CODE_NEXTSTATE_FOUND
	| CODE_INFINITECOST
	| CODE_ERROR;;
let getNextState graph next_x next_y next_state source_state =
	let robot_state =
		if source_state <> (-1) then source_state
		else graph.robotstate in
	let robot_g = get_rhs graph robot_state in
	if robot_g = infinite_cost then
		(print_string "getNextState: g-value of the robot position is infinite.\n";
		print_string "getNextState: There is no path.\n";
		next_state := (-1);
		CODE_INFINITECOST)
	else if (get_searchtree graph robot_state) <> (-1) then
		(let rec find_minimum l min_action n_min = match l with
			| (n,action)::t ->
				(* In general only important when ties are not
					 broken in favor of smaller g values *)
				reevaluatemaxequation graph robot_state action;
				let succstate = CustomGraph.get_edge_maxstate action in
				(* If a smaller state is found *)
				let cost = (CustomGraph.get_edge_cost action) +. (get_g graph succstate) in
				if cost < n_min then find_minimum t action cost
				else if (cost = n_min) then
					(let action_point = CustomGraph.get_point graph.graph
																	(CustomGraph.get_edge_succstate action) in
					let min_action_point = CustomGraph.get_point graph.graph
																	(CustomGraph.get_edge_succstate min_action) in
					if (infinite_cost *. action_point.(0) +. action_point.(1))
						<= (infinite_cost *. min_action_point.(0) +. min_action_point.(1)) then
							find_minimum t action cost
					else
							find_minimum t min_action n_min)
				else
					find_minimum t min_action n_min
			| [] -> (min_action,n_min) in
		let robot_nei = CustomGraph.nei graph.graph robot_state 0 in
		let first_action = snd (List.hd robot_nei) in
		let (min_action,n_min) = find_minimum robot_nei first_action infinite_cost in
		let min_action_point = CustomGraph.get_point graph.graph
						(CustomGraph.get_edge_succstate min_action) in
		(* DEBUG *)
		print_string "min_action_sucessor : ";
		print_int (CustomGraph.get_edge_succstate min_action);
		print_string "\n";
		next_x := min_action_point.(0);
		next_y := min_action_point.(1);
		next_state := CustomGraph.get_edge_succstate min_action;
		CODE_NEXTSTATE_FOUND)
	else CODE_ERROR;;

(* Returns state containing point and which is a possible successor of source_state *)
let getStatebyMazeCoordfromNeighs graph point source_state =
	(* Iterate through successors *)
	let rec browse_succ l = match l with
		| succ::t ->
			let succ_point = CustomGraph.get_point graph.graph succ in
			let succ_rect = CustomGraph.find_rect graph.graph succ_point in
			(* See if the successors state contains the point *)
			if Rect.contains succ_rect point then succ
			else browse_succ t
		| [] -> (-1) in
	(* Iterate through all actions *)
	let rec browse_actions l = match l with
		| (_,action)::t ->
			max (browse_succ (CustomGraph.get_edge_successors action)) (browse_actions t)
		| [] -> (-1) in
	browse_actions (CustomGraph.nei graph.graph source_state 0);;

(* Coordinate of the next successor state (even though it might differ from maxstate) *)
let getNextMazeSquare graph next_maze_state pnext_state =
	let maze_iter = Array.make 2 (-1.) in
	let next_x = ref (-1.) in
	let next_y = ref (-1.) in
	let path_queue = Queue.create () in
	let next_state = ref (-1) in
	(* Get robot's current maze square and state and
		 set it to iterator's square and state *)
	maze_iter.(0)<-graph.robot_x;
	maze_iter.(1)<-graph.robot_y;
	(* Initialise next state *)
	next_maze_state := graph.robotstate;
	pnext_state := (-1);
	if graph.robotstate = graph.robotgoalstate then maze_iter
	else
		(match (getNextState graph next_x next_y next_state graph.robotstate) with
			| CODE_NEXTSTATE_FOUND ->
				pnext_state := !next_state;
				(* Add to the current path a path from iterator's square to the next state *)
				addLocalPathMinimax graph maze_iter graph.robotstate !next_state path_queue;
				let path_size = Queue.length path_queue in
				if path_size = 0 then maze_iter
				else
					(next_maze_state := getStatebyMazeCoordfromNeighs graph
						(Queue.top path_queue) graph.robotstate;
					if !next_maze_state = (-1) then
						failwith "getNextMazeSquare: Next state does not exist!";
					Queue.top path_queue);
			| _ ->
				print_string "getNextMazeSquare: Path to the goal could not be found.\n";
				maze_iter);;

(* Edit/Add actions *)
(* NOTE: Can't be moved to CustomGraph because of equation evaluation call *)
(* Adds a successor state to the list of possible actions for a given action *)
let addstatetoaction graph source action state =
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
				addstatetoaction graph state1 action state2;)
			else browse_actions t
		| [] -> found := false in
	browse_actions (CustomGraph.nei graph.graph state1 0);
	(* Add state1 as predecessor of state2 *)
	CustomGraph.add_edg graph.graph state2 state1
		(CustomGraph.create_edge state1 infinite_cost) 1;
	(* Otherwise just create if *)
	if not !found then
		(let action = CustomGraph.create_edge state2 cost in
		addstatetoaction graph state1 action state2;
		(* Add the action to state1 *)
		CustomGraph.add_edg graph.graph state1 state2 action 0);;


(* Update the key modifier *)
let updateKeyModifier graph =
	let robot_state_point = CustomGraph.get_point graph.graph graph.robotstate in
	(* Initial update *)
	if (!lastKmUpdate_x = (-1.) && !lastKmUpdate_y = (-1.)) then
		(lastKmUpdate_x := robot_state_point.(0);
		lastKmUpdate_y := robot_state_point.(1));
	keymodifier :=
		!keymodifier +. (stateheuristic graph !lastKmUpdate_x !lastKmUpdate_y);
	lastKmUpdate_x := robot_state_point.(0);
	lastKmUpdate_y := robot_state_point.(1);
	(* We use a heap *)
	keymodifier := 0.;;

(* Reset the key modifier *)
let resetKeyModifier graph =
	let robot_state_point = CustomGraph.get_point graph.graph graph.robotstate in
	keymodifier := 0.;
	lastKmUpdate_x := robot_state_point.(0);
	lastKmUpdate_y := robot_state_point.(1);;

(* Split a given state *)
let splitState graph state =
	print_string "splitState: State ";
	print_int state;
	print_string " is split.\n";
	(* Check the graph's validity *)
	check_graph graph;
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
				(addaction graph state1 (get_distance_point state1_point n_point) n;
				addaction graph n (get_distance_point state1_point n_point) state1);
			if Rect.are_adjacent state2_rect n_rect then
				(addaction graph state2 (get_distance_point state2_point n_point) n;
				addaction graph n (get_distance_point state2_point n_point) state2);
			new_edges t
		| [] -> () in
	new_edges old_nei;
	(* Add actions between the two states *)
	addaction graph state1 (get_distance graph state1 state2) state2;
	addaction graph state2 (get_distance graph state1 state2) state1;
	(* Eventually change the robot/goal states *)
	if (Rect.contains state1_rect [|graph.robot_x;graph.robot_y|]) then
		graph.robotstate<-state1;
	if (Rect.contains state2_rect [|graph.robot_x;graph.robot_y|]) then
		graph.robotstate<-state2;
	if (Rect.contains state1_rect [|graph.robot_goal_x;graph.robot_goal_y|]) then
		graph.robotgoalstate<-state1;
	if (Rect.contains state2_rect [|graph.robot_goal_x;graph.robot_goal_y|]) then
		graph.robotgoalstate<-state2;
	(* Remove from the heap *)
	if PriorityQueue.mem graph.queue state then
		remove graph state;
	(* Check the graph's validity *)
	check_graph graph;;

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
			else if (n = graph.robotgoalstate) then
				(* If a bordering state is a goal state then the current state
					 needs to be split *)
				border := true
			else ();
			split_loosers border t
		| [] -> () in
	(* if isRobotStateCut graph then
		(* If the robot state is cut then it is the only state in the
			 region of the lost states and we can just split it and its
			 neighbors split only the robot state and its neighbors *)
		(let border = ref false in
		split_loosers border (CustomGraph.nei graph.graph graph.robotstate 0);
		(* If the cell was a bordering one then split it as welll *)
		if !border then splitState graph graph.robotstate;)
	else *)
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
	(* Check heap validity *)
	check_heap graph;
	(* Recompute the shortest path *)
	dstarstep graph;
	(* Return depending if robot g-value is infinite after resets *)
	not ((get_rhs graph graph.robotstate) = infinite_cost);;

(* Returns false if no solution exists to the existing problem
	 Returns true if a robot successfully executes a step toward the
	 possible solution *)
let step_execute graph =
	(* Move graphic pointer to the robot position *)
	Graphics.moveto (int_of_float graph.robot_x) (int_of_float graph.robot_y);
	let newExperience = ref false in
	let next_state = ref (-1) in
	let next_maze_state = ref (-1) in
	if graph.robotstate = graph.robotgoalstate then true
	else
		(let g = get_rhs graph graph.robotstate in
		(* If no path is found then split cells *)
		if g = infinite_cost then
			(if not (splitBorderCells graph) then
				(* Splitting bordering cells was unsuccessful *)
				(print_string "step_execute: No solution exists.\n";
				false)
			else true)
		else
			((* Get next maze square that the robot has to go into *)
			let maze_coord = getNextMazeSquare graph next_maze_state next_state in
			(* Is the next state is not as expected *)
			let rec get_action dest l = match l with
				| (n,action)::t ->
					if n = dest then action
					else get_action dest t
				| [] -> failwith "get_action: Action not found" in
			let exec_action =
				get_action !next_state (CustomGraph.nei graph.graph graph.robotstate 0) in
			let source_state = graph.robotstate in
			if (!next_maze_state <> graph.robotstate) &&
				(CustomGraph.is_in_successors exec_action !next_maze_state) then
				newExperience := true;
			(* If it is empty then move the robot into it and if it is
				a brand new experience add it to the database *)
					(* Remember the old expected cost plus g of going along this action *)
			let (collide,collision_point) =
				Collision.check_collision graph.scene [|graph.robot_x;graph.robot_y|] maze_coord in
			(if not collide then
				(move_robot graph maze_coord !next_maze_state;
				(if !newExperience then
					(* Add the new experience *)
					(print_string "step_execute: New experience.\n";
					addstatetoaction graph source_state exec_action !next_maze_state;
					(* Make sure that the key was updated *)
					updateKeyModifier graph;
					(* Update vertex for search algorithm *)
					insertifinconsistent graph source_state;
					(* Update the heap *)
					redoheap graph;
					(* Check heap validity *)
					check_heap graph;
					(* Redo the computation *)
					dstarstep graph;)))
			else
				(* Update the graph and recompute shortest path *)
				(print_string "step_execute: Next cell is an obstacle.\n";
				(* addstatetoaction graph graph.robotstate exec_action graph.robotstate; *)
				CustomGraph.set_edge_cost exec_action infinite_cost;
				reevaluatemaxequation graph graph.robotstate exec_action;
				(* Make sure that the key was updated *)
				updateKeyModifier graph;
				(* Update vertex for search algorithm *)
				insertifinconsistent graph graph.robotstate;
				(* Update the heap *)
				redoheap graph;
				(* Check heap validity *)
				check_heap graph;
				(* Redo the computation *)
				dstarstep graph););
			true;););;

(* Setup actions in a graph *)
let setup_actions graph =
	let size = CustomGraph.get_count graph.graph in
	(* Iterate through all the states *)
	for i = 0 to (size-1) do
		for j = 0 to (size-1) do
			let state1_point = CustomGraph.get_point graph.graph i in
			let state1_rect = CustomGraph.find_rect graph.graph state1_point in
			let state2_point = CustomGraph.get_point graph.graph j in
			let state2_rect = CustomGraph.find_rect graph.graph state2_point in
			if (Rect.are_adjacent state1_rect state2_rect) && (i <> j) &&
				not (CustomGraph.adj graph.graph i j 0) then
				addaction graph i (get_distance_point state1_point state2_point) j;
			if (i = j) then addaction graph i infinite_cost j;
		done;
	done;;