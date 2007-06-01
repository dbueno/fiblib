(** This module provides an efficient, imperative implementation of Fibonacci
    heaps. [CLRS 2nd ed. ch. 20]

    Each heap is conceptually a min-heap (elements are higher whose keys are
    less than other keys). However, the comparison function is parameterised by
    functor instantiation, so it is just as easy to create a min-heap as a
    max-heap.

    This library is released under a BSD-style license. See LICENSE.
*)


(** The signature of the output of {!Make}. *)
module type S = sig


  (** {3 Types } *)

  type key
      (** The totally-ordered key type.

          The comparison of this key determines whether this is a min-heap or a
          max-heap. If [fibkey] is [int], for example, and is compared by
          [Pervasives.compare], you get a min-heap.

          In general, if [f] compares keys, it is required that that [f k k' <
          0] if and only if [k] should occur before [k'] in extraction order. *)

  type 'a fibheap
      (** A fibonacci heap containing elements of type ['a]. *)

  type 'a fibnode
      (** A fibonacci heap node. *)

  (** {3 Exceptions } *)

  (** Thrown when the heap is empty. *)
  exception Empty;;

  exception Key_too_big
    (** Thrown when the new key given for a call to {!fibheap_decrease_key}
        would not result in a decreased key. *)

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

        @raise Not_found if [node] is not in the heap
        @raise Key_too_big if [node]'s key is smaller than [new_key] *)


  (** {3 Accessors } *)


  val fibnode_data : 'a fibnode -> 'a

  val fibnode_key : 'a fibnode -> key

  (** {3 Printing } *)

  val fibheap_print : ('a -> string) -> Format.formatter -> 'a fibheap -> unit

end

(** The type of key comparators. *)
module type KeyOrderType = sig
  include Map.OrderedType (* declares type t *)

  val to_string : t -> string
end

(** Creates a Fibonacci heap module with key comparison done by [Ord]. *)
module Make (Ord : KeyOrderType) : S with type key = Ord.t

(* this comment intentionally contentless *)
