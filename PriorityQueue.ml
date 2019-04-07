module PriorityQueue :
  sig
    type 'a order = 'a -> 'a -> bool
    type 'a t
    val make: 'a order -> 'a t
    val length: 'a t -> int
    val is_empty: 'a t -> bool
    val add: 'a t -> 'a -> unit
    val mem: 'a t -> 'a -> bool
    val first: 'a t -> 'a
    val remove_first: 'a t -> unit
    val remove: 'a t -> 'a -> unit
    val clear: 'a t -> unit
    val reorder_up: 'a t -> 'a -> unit
    val reorder_down: 'a t -> 'a -> unit
  end =

  struct
    type 'a order = 'a -> 'a -> bool

    type 'a queue = {
      heap:    'a LocalDynArray.t;
      indices: ('a, int) Hashtbl.t;
      order:   'a order;
    }

    type 'a t = 'a queue

    let make order = {
      heap    = LocalDynArray.make 0 (Obj.magic 0);
      indices = Hashtbl.create 32;
      order   = order;
    }

    let length h =
      LocalDynArray.length h.heap

    let is_empty h =
      length h = 0

    let get h =
      LocalDynArray.unsafe_get h.heap

    let set h i x =
      LocalDynArray.unsafe_set h.heap i x;
      Hashtbl.replace h.indices x i

    let mem h x =
      Hashtbl.mem h.indices x

    let parent i = (i - 1) / 2
    let left i   = 2 * i + 1
    let right i  = 2 * i + 2

    let has_left h i =
      left i < length h

    let has_right h i =
      right i < length h

    let is_heap h =
      let ord = h.order in
      let rec is_heap i =
           (not (has_left h i) ||
              (ord (get h i) (get h (left i)) && is_heap (left i)))
        && (not (has_right h i) ||
              (ord (get h i) (get h (right i)) && is_heap (right i)))
      in
        is_heap 0

    let down_heap h i =
      let x = get h i in
      let ord = h.order in
      let rec down_heap j =
        if has_left h j then
          let l = left j in
          let r = right j in
          let k =
            if has_right h j && not (ord (get h l) (get h r)) then r else l in
          let y = get h k in
            if ord x y then
              set h j x
            else begin
              set h j y;
              down_heap k
            end
        else if j <> i then
          set h j x
      in
        down_heap i

    let up_heap h i =
      let x = get h i in
      let ord = h.order in
      let rec up_heap j =
        let k = parent j in
        let y = get h k in
          if j = 0 || ord y x then
            set h j x
          else begin
            set h j y;
            up_heap k
          end
      in
        up_heap i

    let make_heap h =
      for i = (length h) / 2 - 1 downto 0 do
        down_heap h i
      done

    let first h =
      if is_empty h then failwith "PriorityQueue.first: empty queue"
      else get h 0

    let add h x =
      let i = length h in
        LocalDynArray.add h.heap x ;
        Hashtbl.add h.indices x i;
        up_heap h i

    let remove_index h i =
      let x = get h i in
      let y = get h (length h - 1) in
        set h i y;
        LocalDynArray.remove_last h.heap;
        Hashtbl.remove h.indices x;
        down_heap h i

    let remove_first h =
      if is_empty h then failwith "PriorityQueue.first: empty queue"
      else remove_index h 0

    let remove h x =
      try remove_index h (Hashtbl.find h.indices x)
      with Not_found -> ()

    let clear h =
      LocalDynArray.clear h.heap;
      Hashtbl.clear h.indices

    let reorder_up h x =
      try up_heap h (Hashtbl.find h.indices x)
      with Not_found -> ()

    let reorder_down h x =
      try down_heap h (Hashtbl.find h.indices x)
      with Not_found -> ()
end