(* Fibonacci heap unit tests. *)

open OUnit;;

(* FibHeap_full gives us access to all the functions in fibHeap, so we can unit
   test them. *)
module IntMinHeap = FibHeap_full.Make (struct
  type t = int
  let compare = Pervasives.compare
  let to_string = string_of_int
end);;
open IntMinHeap;;
let print_heap = fibheap_print string_of_int Format.std_formatter;;

let to_listr node = fold_right (fun d l -> d :: l) node [];;

let init_list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;

(** Heaps initialised in the set up bracket, and passed to each unit test. *)
type testing_heaps = {
    small_heap : int fibheap;
};;

let assert_int_equal = assert_equal ~printer:string_of_int;;

let tests = TestList
  (* Bracket each test by reinitialising test variables. *)
  (List.map
      (fun (name, test_fun) ->
        name >::
          (bracket
              (fun () ->
                let small_heap = fibheap_create () in
                  List.iter
                    (fun i -> ignore (fibheap_insert small_heap i i))
                    init_list;
                  {small_heap = small_heap})
              test_fun
              (fun _ -> ())))
[

"splice",
(fun _ ->
  let n = create_fibnode 1 1 in
  let n' = create_fibnode 2 2 in
  let n'' = create_fibnode 3 3 in
  let n''' = create_fibnode 4 4 in
    assert_bool "" (n.left == n.right);

    splice n' n;
    (* n <--> n' *)

    assert_bool "n.l == n'" (n.left == n');
    assert_bool "n.r == n'" (n.right == n');
    assert_bool "n'.l == n" (n'.left == n);
    assert_bool "n'.r == n" (n'.right == n);

    splice n' n'';
    (* n <--> n' <--> n'' *)

    assert_bool "n.r == n'" (n.right == n');
    assert_bool "n.l == n''" (n.left == n'');
    assert_bool "n'.l == n" (n'.left == n);
    assert_bool "n'.r == n''" (n'.right == n'');
    assert_bool "n''.l == n'" (n''.left == n');
    assert_bool "n''.r == n" (n''.right == n);

    splice n'' n''';
    (* n <--> n' <--> n'' <--> n''' *)

    assert_bool "n.r == n'" (n.right == n');
    assert_bool "n.l == n'''" (n.left == n''');
    assert_bool "n'.l == n" (n'.left == n);
    assert_bool "n'.r == n''" (n'.right == n'');
    assert_bool "n''.l == n'" (n''.left == n');
    assert_bool "n''.r == n'''" (n''.right == n''');
    assert_bool "n'''.l == n''" (n'''.left == n'');
    assert_bool "n'''.r == n" (n'''.right == n);
);

"fold_right",
(fun _ ->
  let n = create_fibnode 1 1 in
    splice n (create_fibnode 2 2);
    splice (create_fibnode 3 3) n;

    assert_int_equal 3 (fold_right (fun node acc -> acc + 1) n 0));

"iter_right",
(fun _ ->
  let n1 = create_fibnode 1 1 in
  let n2 = create_fibnode 2 2 in
  let n3 = create_fibnode 3 3 in
  let counter = ref 0 in
    splice n2 n1;
    splice n1 n3;

    counter := 0;
    iter_right (fun _ -> incr counter) n1;
    assert_int_equal 3 !counter;

    counter := 0;
    iter_right (fun _ -> incr counter) n2;
    assert_int_equal 3 !counter;

    counter := 0;
    iter_right (fun _ -> incr counter) n3;
    assert_int_equal 3 !counter;
);

"iter_left",
(fun _ ->
  let n1 = create_fibnode 1 1 in
  let n2 = create_fibnode 2 2 in
  let n3 = create_fibnode 3 3 in
  let counter = ref 0 in
    splice n2 n1;
    splice n3 n1;

    counter := 0;
    iter_left (fun _ -> incr counter) n1;
    assert_int_equal 3 !counter;

    counter := 0;
    iter_left (fun _ -> incr counter) n2;
    assert_int_equal 3 !counter;

    counter := 0;
    iter_left (fun _ -> incr counter) n3;
    assert_int_equal 3 !counter;
);

"safe_iter",
(fun _ ->
  let n1 = create_fibnode 1 1 in
  let n2 = create_fibnode 2 2 in
  let n3 = create_fibnode 3 3 in
  let counter = ref 0 in
    splice n2 n1;
    splice n1 n3;

    counter := 0;
    safe_iter (fun _ -> incr counter) n1;
    assert_int_equal 3 !counter;

    counter := 0;
    safe_iter (fun _ -> incr counter) n2;
    assert_int_equal 3 !counter;

    counter := 0;
    safe_iter (fun _ -> incr counter) n3;
    assert_int_equal 3 !counter;
);

"to_list",
(fun _ ->
  let n = create_fibnode 1 1 in
    splice (create_fibnode 2 2) n;
    splice n (create_fibnode 3 3);

    let l = to_list n in
      assert_int_equal 3 (List.length l));

"remove1",
(fun _ ->
  let n = create_fibnode 1 1 in
  let n' = create_fibnode 2 2 in
  let n'' = create_fibnode 3 3 in
  let n''' = create_fibnode 4 4 in
    splice n' n;
    splice n'' n';
    splice n''' n'';
    remove n''';
    assert_int_equal 3 (List.length (to_list n));
    assert_int_equal 3 (List.length (to_listr n)););

"remove2",
(fun _ ->
  let n = create_fibnode 1 1 in
  let n' = create_fibnode 2 2 in
  let n'' = create_fibnode 3 3 in
  let n''' = create_fibnode 4 4 in
    splice n' n;
    splice n'' n';
    splice n''' n'';
    remove n''';
    assert_int_equal 3 (List.length (to_list n')));

"remove3",
(fun _ ->
  let n = create_fibnode 1 1 in
  let n' = create_fibnode 2 2 in
  let n'' = create_fibnode 3 3 in
  let n''' = create_fibnode 4 4 in
    splice n' n;
    splice n'' n';
    splice n''' n'';
    remove n''';
    assert_int_equal 3 (List.length (to_list n'')));

"swap",
(fun _ ->
  let x = ref 1 in
  let y = ref 2 in
    FibHeap_full.swap x y;
    assert_int_equal 1 !y;
    assert_int_equal 2 !x;);

"size small heap",
(fun {small_heap=heap} ->
  assert_int_equal (fibheap_size heap) (List.length init_list));

"extract min small heap",
(fun {small_heap=heap} ->
(*   print_endline "** Initial heap:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 1 (fibheap_extract_min heap);
(*   print_endline "** After first extract_min:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 2 (fibheap_extract_min heap);
(*   print_endline "** After second:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 3 (fibheap_extract_min heap);
(*   print_endline "** After third:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 4 (fibheap_extract_min heap);
(*   print_endline "** After fourth:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 5 (fibheap_extract_min heap);
(*   print_endline "** After fifth:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 6 (fibheap_extract_min heap);
(*   print_endline "** After sixth:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 7 (fibheap_extract_min heap);
(*   print_endline "** After seventh:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 8 (fibheap_extract_min heap);
(*   print_endline "** After eighth:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 9 (fibheap_extract_min heap);
(*   print_endline "** After ninth:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 10 (fibheap_extract_min heap);
(*   print_endline "** After tenth:"; flush stdout; *)
(*   print_heap heap; *)

  try
    fibheap_extract_min heap;
    assert_failure ("heap not Empty")
  with IntMinHeap.Empty -> ();
);

"extract min small heap munge",
(fun {small_heap=heap} ->
  assert_int_equal 1 (fibheap_extract_min heap);
  assert_int_equal 2 (fibheap_extract_min heap);

  fibheap_insert heap 1 1;
  assert_int_equal 1 (fibheap_extract_min heap);
  assert_int_equal 3 (fibheap_extract_min heap);

  fibheap_insert heap 11 11;
  fibheap_insert heap 1 1;
  fibheap_insert heap 1 1;
  assert_int_equal 1 (fibheap_extract_min heap);
  assert_int_equal 1 (fibheap_extract_min heap);
  assert_int_equal 4 (fibheap_extract_min heap);
  assert_int_equal 5 (fibheap_extract_min heap);
  assert_int_equal 6 (fibheap_extract_min heap);
  assert_int_equal 7 (fibheap_extract_min heap);
  assert_int_equal 8 (fibheap_extract_min heap);
  assert_int_equal 9 (fibheap_extract_min heap);
  assert_int_equal 10 (fibheap_extract_min heap);
  assert_int_equal 11 (fibheap_extract_min heap);

  try
    fibheap_extract_min heap;
    assert_failure ("heap should be Empty")
  with IntMinHeap.Empty -> ();
);

"stress",
(fun _ ->
  let heap = fibheap_create () in
  let n = 10000 in
    for i = 0 to n - 1 do
      fibheap_insert heap i i;
    done;
    for i = 0 to n - 1 do
      assert_int_equal (n - i) (fibheap_size heap);
      assert_int_equal i (fibheap_extract_min heap);
    done;
);

"decrease key",
(fun {small_heap=heap} ->
  ()
);

]);;


run_test_tt ~verbose:true tests;;
