type key
type 'a heap
type 'a node

exception Empty
exception UnsupportedOperation

val create : int -> 'a heap

val add : 'a heap -> 'a node -> unit

val pop_min : 'a heap -> 'a node

val peek_min : 'a heap -> 'a node

val delete : 'a heap -> 'a node -> unit

val decrease_key : 'a heap -> 'a node -> key -> unit

val is_empty : 'a heap -> bool

val size : 'a heap -> int


val node_new : key:key -> data:'a -> 'a node

val node_data : 'a node -> 'a

val node_key : 'a node -> key


val print : ('a -> string) -> Format.formatter -> 'a heap -> unit
