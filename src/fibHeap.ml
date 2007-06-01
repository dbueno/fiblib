open Format;;

    (*
      directory /Users/Shared/ocaml/extlib-1.5
      load_printer "debug_printers.cma"
      install_printer "print_int_dllist"
    *)

(** The Fibonacci heap implementation. *)


(** Swap the contents of the given ref cells. *)
let swap x y =
  let temp = !x in
    x := !y;
    y := temp;;

module Option = struct
  exception No_value;;
  let get opt =
    match opt with
    | Some x -> x
    | None -> raise No_value;;
  let is_some opt =
    match opt with
    | Some _ -> true
    | _ -> false;;
end;;



(** The signature of the output of {!Make}. *)
module type S = sig


  (** {3 Types } *)

  type key
      (** The totally-ordered key type.

          The comparison of this key determines whether this is a min-heap or a
          max-heap. If [fibkey] is [int], for example, and is compared by
          [Pervasives.compare], you get a min-heap.

          In general, if [f] compares keys, it should be that [f k k' <= 0] if
          and only if [k] should occur higher in the heap than [k']. *)

  type 'a fibheap
      (** A fibonacci heap containing elements of type ['a]. *)

  type 'a fibnode
      (** A fibonacci heap node. *)

  (** {3 Exceptions } *)

  (** Thrown when the heap is empty. *)
  exception Empty

  exception Key_too_big

  (** {3 Operations } *)

  val fibheap_create : unit -> 'a fibheap
    (** Create a fibonacci heap. *)

  val fibheap_insert : 'a fibheap -> 'a -> key -> 'a fibnode
    (** [fibheap_insert heap data key] inserts [data] into [heap], and
        associates it with key [key]. The node created by the insert operation is
        returned. *)

  val fibheap_size : 'a fibheap -> int
    (** @return the number of elements in the heap. *)

  val fibheap_extract_min : 'a fibheap -> 'a
    (** [fibheap_extract_min heap] extracts the item with the minimum key from
        [heap]. *)

  val fibheap_decrease_key : 'a fibheap -> 'a fibnode -> key -> unit
    (** [fibheap_decrease_key heap node new_key] decreases the value of the key
        paired with [node] in the [heap].

        @raise Not_found if [node] is not in the heap *)


  (** {3 Accessors } *)


  val fibnode_data : 'a fibnode -> 'a

  val fibnode_key : 'a fibnode -> key

  (** {3 Printing } *)

  val fibheap_print : ('a -> string) -> Format.formatter -> 'a fibheap -> unit

end

module type KeyOrderType = sig
  include Map.OrderedType

  val to_string : t -> string
end


(** In this module, we speak natively in terms of a min-heap, but this is only
    an implementation detail that keeps me consistent everywhere nodes need
    ordering. Ordering is determined by the [Ord] parameter to this functor; thus
    you can get a min- or max-heap depending on that ordering.
*)
module Make (Ord : KeyOrderType) = struct

  type key = Ord.t

  type 'a fibheap = {
    mutable min : 'a fibnode option;
    (** The current optimal element. *)

    mutable n : int;
    (** The number of elements in the heap. *)

    mutable num_marked : int;
  }

  (** Fibonacci heap doubly-linked list node data structure. This is pretty much
      lifted (or inferred) from the description in CLRS. Note that with doubly-linked,
      circlar lists (like these) a "list" is referred to by *any* node in the
      list. This each fibonacci heap node can represent the fibonacci heap node list.

      Each heap node has a key (for ordering), a parent list, a child list, the
      left and right elements in its list, a mark, a degree, and the data associated
      with this node.  *)
  and 'a fibnode =
      {mutable key : key;
       mutable parent : 'a fibnode option;
       mutable child : 'a fibnode option;

       (* We have to use left and right fields instead of an external
          linked-list library in order to acheive the proper complexity of a Fibonacci
          heap, and to keep the code tidy -- we need to be able to know which info is tied
          with which nodes in the linked list. *)
       mutable left : 'a fibnode;
       mutable right : 'a fibnode;

       mutable mark : bool;
       (** Has this node lost a child since the last time it was made the
           child of another node? *)

       mutable degree : int;
       (** How many levels of kids. *)

       data : 'a;
      };;


  exception InternalError;;
  exception Todo;;

  (** Thrown when an operation requires the {!min} node to be present, but it is
      not. *)
  exception Empty;;
  exception Key_too_big;;


  (* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv *)
  (* Utilities *)

  let fibnode_key {key=key} = key;;
  let fibnode_data {data=data} = data;;


  (** Make a node containing given fibonacci heap key & data. *)
  let create_fibnode data key =
    let rec n = {key = key;
                 data = data;
                 parent = None;
                 child = None;
                 left = n;
                 right = n;
                 mark = false;
                 degree = 0}
    in
      n;;

  (** Connects [n] and [n'] so that [n.right == n' && n'.left == n]. *)
  let splice n n' =
    let right = n.right in
    let left = n'.left in
      n.right <- n';
      n'.left <- n;
      right.left <- left;
      left.right <- right;;


  (** Remove [n] from the dllist of which it is a part, properly linking up the
      nodes surrounding [n], if any. [node] will only point to itself on return. *)
  let remove node =
    let right = node.right in
    let left = node.left in
      left.right <- right;
      right.left <- left;
      (* Make node only point to itself. *)
      node.right <- node;
      node.left <- node;;

  (** Fold over a {!fibnode} list, traversing {!right} pointers.

      N.B. It is {b not} safe to modify the structure of the list during
      iteration. Strange things may happen. *)
  let fold_right f node init =
    let rec loop acc n =
      if n == node.left then
	f n acc
      else
	loop (f n acc) n.right
    in
      loop init node;;

  (** Fold over a {!fibnode} list, traversing {!left} pointers.

      N.B. It is {b not} safe to modify the structure of the list during
      iteration. Strange things may happen.*)
  let fold_left f init node =
    let rec loop acc n =
      if n == node.right then
	f acc n
      else
	loop (f acc n) n.left
    in
      loop init node;;


  (** Iterate over a fibonacci heap node list. Implemented in terms of
      {!fold_right}, so see the warning for {!fold_right}. *)
  let iter_right f node = fold_right (fun x () -> f x) node ();;

  (** Iterate over a fibonacci heap node list. Implemented in terms of
      {!fold_left}, so see the warning for {!fold_left}. *)
  let iter_left f node = fold_left (fun () x -> f x) () node;;

  (** Convert a {!fibnode} list into an OCaml [list] of {!fibnode}s. *)
  let to_list node = fold_left (fun l d -> d :: l) [] node;;

  (** Iterate over the {!fibnode} list, allowing structure frobbing during
      iteration. *)
  let safe_iter f n = List.iter f (to_list n);;



  (** [iterate_fold start times f base] folds over a range, from [start] to
      [times - 1].

      @param start the first index
      @param times the number of times to execute [f]
      @param base the initial value passed to [f] *)
  let rec iterate_fold start times (f : 'a -> int -> 'a) (base : 'a) =
    if start >= times
    then base
    else iterate_fold (start + 1) times f (f base start);;

  (** {5 Comparators for keys } *)

  let ( =* ) k k' = Ord.compare k k' = 0;;
  let ( <* ) k k' = Ord.compare k k' < 0;;
  let ( <=* ) k k' = k <* k' || k =* k';;
  let ( >=* ) k k' = not (k <* k');;
  let ( >* ) k k' = not (k <=* k');;

  (** {3 Operations } *)

  (**
     ------------------------------------------------------------
     CREATE
     ------------------------------------------------------------
  *)
  let fibheap_create () = {
    min = None;
    n = 0;
    num_marked = 0;
  };;


  (**
     ------------------------------------------------------------
     SIZE
     ------------------------------------------------------------
  *)
  let rec fibheap_size heap = heap.n

  (**
     ------------------------------------------------------------
     INSERT
     ------------------------------------------------------------
  *)
  and fibheap_insert heap data key =
    (* Given a fibdata_node insert it appropriately into the heap.

       This function will set the {!min} to the appropriate value; also {!n}
       gets bumped up by one. *)
    let insert_node heap new_fibnode =
      root_list_add heap new_fibnode;

      (* There will be a min due to the root_list_add above.

         If the min's key indicates lower priority, set new_fibdata_node as the
         heap.min. *)
      (* Replace {!min} if necessary. *)
      (if new_fibnode.key <* (Option.get heap.min).key then
        heap.min <- Some new_fibnode);

      heap.n <- heap.n + 1
    in

    let new_node = create_fibnode data key in
      insert_node heap new_node;
      new_node

  (**
     ------------------------------------------------------------
     EXTRACT MIN
     ------------------------------------------------------------
  *)
  and fibheap_extract_min heap =
    match heap.min with
    | None -> raise Empty
    | Some min_fibnode ->
        root_list_add_immediate_children heap min_fibnode;

        if is_alone min_fibnode then
          heap.min <- None
        else
          (heap.min <- Some min_fibnode.right;
           remove min_fibnode;
           consolidate heap);

        heap.n <- heap.n - 1;
        min_fibnode.data

  (**
    ------------------------------------------------------------
    DECREASE KEY
    ------------------------------------------------------------
  *)
  and fibheap_decrease_key heap node new_key =
    if new_key >* node.key then
      raise Key_too_big;

    node.key <- new_key;
    (match node.parent with
    | Some y ->
        if node.key <* y.key then
          (fibheap_cut heap node y;
           fibheap_cascading_cut heap y)
    | None -> ());
    if node.key <* (Option.get heap.min).key then
      heap.min <- Some node;

  (**
     ------------------------------------------------------------
     PRINT
     ------------------------------------------------------------
  *)
  and fibheap_print val_printer fmt heap =
    pp_print_string fmt ("Heap on " ^ string_of_int heap.n ^ " nodes,");
    pp_print_string fmt (" min: " ^
                          match heap.min with
                          | None -> "<none>"
                          | Some m -> val_printer m.data);
    pp_print_newline fmt ();
    pp_print_string fmt ("  Root list: ");
    pp_print_newline fmt ();
    pp_open_hovbox fmt 2;
    (match heap.min with
    | None -> ()
    | Some m -> fibnode_list_print val_printer fmt m);
    pp_close_box fmt ();
    pp_print_newline fmt ();

  (** Print the node list starting with the node [list], recursively printing
      any children of nodes in [list]. *)
  and fibnode_list_print val_printer fmt list =
    pp_print_string fmt "(|";
    pp_print_space fmt ();
    iter_right
      (fun node ->
        pp_print_string fmt (val_printer node.data);

        pp_open_hovbox fmt 6;
        (* If this node has a child, recursively print. *)
        (match node.child with
        | None -> ()
        | Some c ->
            pp_print_string fmt " -> ";
            fibnode_list_print val_printer fmt c);
        pp_close_box fmt ();

        pp_print_space fmt ())
      list;
    pp_print_string fmt "|)";



  (* ---------------------------------------------------------------------- *)
  (* Specific Utilities *)

  and fibheap_cut heap x y =
    remove x;
    y.degree <- y.degree - 1;
    root_list_add heap x;
    x.parent <- None;
    x.mark <- false;

  and fibheap_cascading_cut heap y =
    match y.parent with
    | Some z ->
        if not y.mark then
          y.mark <- true
        else
          (fibheap_cut heap y z;
           fibheap_cascading_cut heap z)
    | None -> ()

  (** Add [new_fibnode] to the root list of [heap], doing the right there if
      there are no nodes in the root list. *)
  and root_list_add heap new_fibnode =
    match heap.min with
    | None -> heap.min <- Some new_fibnode
    | Some min_node -> splice new_fibnode min_node


  (** @return [true] if and only if the {!left} and {!right} of this node are
      identical with this node. *)
  and is_alone fibnode = fibnode.right == fibnode && fibnode.left == fibnode

  (** If [node] has any children, add them to the rootlist of [heap]. If not, do
      nothing.

      Postconditions of [root_list_add_immediate_children heap node]:

      Let [node'] be the [node] after execution.

      - [node'.child] == [node.child]
      - Each child in {!child}'s list has no {!parent}. *)
  and root_list_add_immediate_children heap node =
    match node.child with
    | None -> ()
    | Some child ->
        safe_iter (fun child -> child.parent <- None) child;
        (* Since all the children are in a doubly-linked, circular list, we just
           add child to the root list and the entire child list is spliced in. *)
        root_list_add heap child


  (* Fibonacci heap consolidation.
   ****************************************************

   * This code will blow your mind. View at your own risk. *)

  and max_degree heap =
    let log2 x = log x /. log 2. in
      match heap.n with
        (* This is an InternalError because we shouldn't call this function
           unless heap.n > 0. *)
      | 0 -> raise InternalError
      | n -> int_of_float (log2 (float_of_int n))

  (** Make [new_child] a new child of [parent], and adjust [parent.degree] and
      [new_child.mark] accordingly. *)
  and link ~child:new_child ~parent:parent =
    remove new_child;
    add_child ~parent:parent ~child:new_child;
    parent.degree <- parent.degree + 1;
    new_child.mark <- false;

  (** Make [new_child] a child of [parent]; and set the parent pointer of
      [new_child] to [parent]. Does *not* modify any degrees of [parent] or
      [new_child]. Evaluates to the [parent] with [new_child] added. *)
  and add_child ~parent:parent ~child:new_child =
    (match parent.child with
    | None -> parent.child <- Some new_child
    | Some child_list -> splice new_child child_list);
    new_child.parent <- Some parent


  (** Consolidation works in two steps. *)
  and consolidate heap =
    let a = Array.make (1 + max_degree heap) None in
      (* This loop builds up entries in the array [a]:

         Postcondition: a.(i) contains the *unique* heap tree that has degree
         [i], if there is one. *)
      safe_iter
        (fun w ->
          let d = ref w.degree in
          let x = ref w in

            (* While there is another tree in the heap of the same degree, link
               them: make one a child of the other, depending on the ordering.

               This will increase the degree of the linked tree; that's why we
               loop on [d]. *)
            while Option.is_some a.(!d) do
              (* By construction, if we're here, [a.(!d)] is a tree that has the
                 same degree as [x]. *)
              let it = ref (Option.get a.(!d)) in
                (* This swapping always keeps the node with higher degree -- the
                   parent -- in the x ref cell. *)
                (if !it.key <* !x.key then swap x it);
                link ~child:!it ~parent:!x;
                (* x now has its degree increased by one. *)
                a.(!d) <- None;
                incr d;
            done;
             (* make sure !x doesn't connect to any other members of a *)
            remove !x;

            a.(!d) <- Some !x;)
        (Option.get heap.min);

      heap.min <- None;

      (* This loop reorganises the heap according to the array [a]. *)
      Array.iter
        (fun i -> match i with
        | None -> ()
        | Some fibnodei ->
            assert (fibnodei.left == fibnodei);
            assert (fibnodei.right == fibnodei);
            root_list_add heap fibnodei;

            (* There will always be a {!min} because of the previous
               [root_list_add]. *)
            if fibnodei.key <* (Option.get heap.min).key then
              heap.min <- Some fibnodei)
        a;


  and print_consolidate_array a =

      (* Print array a. *)
      pp_open_hovbox err_formatter 2;
      pp_print_string err_formatter "[| ";
      for i = 0 to Array.length a - 1 do
        let f = err_formatter in
          (match a.(i) with
          | None -> pp_print_string f "<none>";
          | Some i ->
              pp_print_string f (Ord.to_string i.key);
              pp_print_string f "[";
              pp_print_string f (string_of_int i.degree);
              pp_print_string f "]");
          pp_print_string f ";";
          pp_print_space f ();
      done;
      pp_print_string err_formatter "|]";
      pp_print_newline err_formatter ();
      pp_close_box err_formatter ();
      pp_print_flush err_formatter ();
      flush stderr;

end;;
