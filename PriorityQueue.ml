(* Files de priorité min *)
(* Marc Lorenzi *)
(* Février 2018 *)

exception Empty_Queue;;
exception Full_Queue;;

module PriorityQueue :
  sig
    type 'a t
    val reset_stats : unit -> unit
    val get_stats : unit -> int * int * int
    val create : int -> 'a -> 'a t
    val is_empty : 'a t -> bool
    val is_in : 'a -> 'a t -> bool
    val top : 'a t -> 'a * int
    val pop : 'a t -> 'a * int
    val push : 'a * int -> 'a t -> unit
    val decrease_prio : 'a * int -> 'a t -> unit
  end =
  struct
		type 'a t = {
				maxsize : int;
				mutable size : int;
				data : 'a array;
				prio : int array;
				index : ('a, int) Hashtbl.t 
		};;

		(* -------------------------------------------- *)
		(* Statistiques sur l'utilisation des files *)

		type stat = {
				mutable nbpush : int;
				mutable nbpop : int;
				mutable bigsize : int
		};;

		let global_stat = {
				nbpush = 0;
				nbpop = 0;
				bigsize = 0
		};;

		let reset_stats () =
				global_stat.nbpush <- 0;
				global_stat.nbpop <- 0;
				global_stat.bigsize <- 0;;

		let get_stats () =
				(global_stat.nbpush, global_stat.nbpop, global_stat.bigsize);;

		(* Crée une nouvelle file de taille maximale n *)

		let create n init = {
				maxsize = n;
				size = 0;
				data = Array.make n init;
				prio = Array.make n 0;
				index = Hashtbl.create ~random:false 128
		};;

		(* La file est-elle vide ? *)
		let is_empty f = f.size = 0;;

		(* l'objet x est-il dans la file f ? *)
		let is_in x f =
				try
				    let _ = Hashtbl.find f.index x in true
				with
				    Not_found -> false;;

		(* index de l'objet x dans la file f *)
		let index_of x f =
				try
				    Hashtbl.find f.index x
				with
				    Not_found -> raise Not_found;;


		(* Taille de la file f *)

		let length f = f.size;;

		(* Echange les éléents d'indices i et j dans le tableau t *)

		let exchange_array t i j =
				let tmp = t.(i) in
				t.(i) <- t.(j);
				t.(j) <- tmp;;

		(* Echange les éléments d'indices x et y dans la table t *)

		let exchange_hash t x y =
				let i = Hashtbl.find t x
				and j = Hashtbl.find t y in
				Hashtbl.replace t x j;
				Hashtbl.replace t y i;;
		 
		(* Echange les éléments d'indices i et j dans la file f *)

		let exchange f i j =
				exchange_array f.data i j;
				exchange_array f.prio i j;
				exchange_hash f.index f.data.(i) f.data.(j);;

		(* Elément de priorité minimale dans la file f *)

		let top f = 
				if f.size > 0 then    
				    (f.data.(0), f.prio.(0))
				else raise Empty_Queue;;

		(* père, fils gauche, fils droit *)

		let father k = (k - 1) / 2;;
		let left k = 2 * k + 1;;
		let right k = 2 * k + 2;;

		(* Mettre à la bonne position dans la file un élément dont la
		priorité risque d'être plus grande que celle de l'un de ses fils *)

		let rec bubble_down f k =
				let m = ref k in
				if left k < f.size && f.prio.(left k) < f.prio.(k) then
				    m := left k;
				if right k < f.size && f.prio.(right k) < f.prio.(!m) then
				    m := right k;
				if !m <> k then (
				    exchange f !m k;
				    bubble_down f !m
				);;
			
		(* Mettre à la bonne position dans la file un élément dont la
		priorité risque d'être plus petite que celle de son père *)

		let rec bubble_up f k =
				let m = ref k in
				if !m > 0 && f.prio.(father !m) > f.prio.(!m) then 
				    m := father k;
				if k <> !m then (
				    exchange f k !m;
				    bubble_up f !m
				);;

		(* Déplacer dans la file f l'élément d'indice i vers l'indice j *)

		let move f i j =
				if i <> j then (
				    Hashtbl.remove f.index f.data.(j);
				    f.data.(j) <- f.data.(i);
				    f.prio.(j) <- f.prio.(i);
				    Hashtbl.replace f.index f.data.(j) j
				);;

		(* Supprime de la file f l'élément de priorité minimale.
		Renvoie cet élement ainsi que sa priorité *)

		let pop f =
				global_stat.nbpop <- global_stat.nbpop + 1;
				if f.size = 0 then raise Empty_Queue
				else
				    let x = f.data.(0)
				    and p = f.prio.(0) in
				    if f.size > 1 then (
				        move f (f.size - 1) 0;
				        f.size <- f.size - 1;
				        bubble_down f 0
				    )
				    else (
				        f.size <- 0;
				        Hashtbl.remove f.index x
				    );
				    (x, p);;

		(* Ajoute à f l'objet x de priorité p *)

		let push (x, p) f =
				global_stat.nbpush <- global_stat.nbpush + 1;
				if f.size = f.maxsize then raise Full_Queue
				else (
				    f.size <- f.size + 1;
				    if f.size > global_stat.bigsize then global_stat.bigsize <- f.size;       
				    f.data.(f.size - 1) <- x;
				    f.prio.(f.size - 1) <- p;
				    Hashtbl.replace f.index x (f.size - 1);
				    bubble_up f (f.size - 1)
				);;

		(* Diminue la priorité de l'objet x à la valeur p *)       
		(* Ne fait rien si la nouvelle priorité est supérieure
		à l'ancienne *)

		let decrease_prio (x, p) f =
				let k = Hashtbl.find f.index x in
				if f.prio.(k) > p then (
				    f.prio.(k) <- p;
				    bubble_up f k
				);;
		 
		(* Augmente la priorité de l'objet x à la valeur p *)       
		(* Ne fait rien si la nouvelle priorité est inférieure
		à l'ancienne *)

		let increase_prio (x, p) f =
				let k = Hashtbl.find f.index x in
				if f.prio.(k) < p then (
				    f.prio.(k) <- p;
				    bubble_down f k
				);;

		(* Modifie la priorité de l'objet x à la valeur p *)       
		(* Ne fait rien si la nouvelle priorité est identique
		à l'ancienne *)

		let modify_prio (x, p) f =
				let k = Hashtbl.find f.index x in
				if f.prio.(k) < p then increase_prio (x, p) f
				else if f.prio.(k) > p then decrease_prio (x, p) f
				else ();;
	end


