type key
type 'a heap
type 'a node

exception Empty
exception UnsupportedOperation

val create : unit -> 'a heap

val insert : 'a heap -> 'a node -> unit

val extract_min : 'a heap -> 'a node

val decrease_key : 'a heap -> 'a node -> key -> unit

val size : 'a heap -> int


val node_new : key:key -> data:'a -> 'a node
val node_data : 'a node -> 'a
val node_key : 'a node -> key

val print : ('a -> string) -> Format.formatter -> 'a heap -> unit
