(* Self-contained test case for doubly-linked list troubles. *)


type 'a dllnode = {key : int;
                   right : 'a dllnode Lazy.t;
                   left : 'a dllnode Lazy.t;
                   parent : 'a dllnode option;
                   child : 'a dllnode option;
                   mark : bool;
                   degree : int;
                   data : 'a;
};;

let rec empty = {key = 0; left = lazy empty; right = lazy empty; parent = None; child = None; mark = false; degree = 0; data = 0};;

let make_proper_node data =
  let rec l = {empty with left = lazy l; right = lazy l; data = data} in
    l

and node_ll_add llnode new_node =
  (* ... <-> llnode <-> former_right <-> ...

     =>

     ... <-> llnode <-> new_node <-> former_right <-> ...
  *)
  if Lazy.force llnode.right == llnode
    (* there is only 1 node in this linked list. *)
  then
    let rec new_node' = {new_node with right = lazy other; left = lazy other}
    and other = {llnode with right = lazy new_node'; left = lazy new_node'} in
      new_node'

  else
    let former_right = Lazy.force llnode.right in
    let rec new_node' =
      {new_node with
        right = lazy {former_right with left = lazy new_node'};
        left = lazy {llnode with right = lazy new_node'}} in
      new_node'
;;


let test () =
  let node = make_proper_node 0 in
  let _ = assert (Lazy.force node.left == Lazy.force node.right) in
  let _ = assert (Lazy.force node.left == node) in
  let _ = assert (node.data = 0) in
  let node = node_ll_add node (make_proper_node 1) in
  let _ = assert ((Lazy.force node.left).data = 0) in
  let _ = assert (Lazy.force node.left == Lazy.force node.right) in
  let _ = assert (not (Lazy.force node.left == node)) in
  let _ = assert (Lazy.force (Lazy.force node.left).right == node) in
  let _ = assert (Lazy.force (Lazy.force node.right).left == node) in
  let _ = assert (Lazy.force (Lazy.force node.right).right == node) in
  let _ = assert (Lazy.force (Lazy.force node.left).left == node) in
    ()
;;

test ();;
