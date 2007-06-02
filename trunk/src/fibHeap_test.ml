(* Fibonacci heap unit tests. *)

open OUnit;;

(* FibHeap_full gives us access to all the functions in fibHeap, so we can unit
   test them. *)

(** IntMinHeap is a min-heap using ints as keys and data. *)
module IntMinHeap = FibHeap_full.Make (struct
  type t = int
  let compare = Pervasives.compare
end);;

open IntMinHeap;;


(** {4 Utilities } *)
let print_heap = fibheap_print string_of_int Format.std_formatter;;
let to_listr node = fold_right (fun d l -> d :: l) node [];;
let assert_int_equal = assert_equal ~printer:string_of_int;;
let assert_data_equal x act =
  if x <> act then
    assert_failure ("Expected data " ^ string_of_int x ^ " but was "
                     ^ string_of_int act);;
let rec iota n =
  match n with
  | 0 -> []
  | _ -> n - 1 :: iota (n - 1);;

(** {4 Data } *)
let init_list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;



(** Heaps initialised in the set up bracket, and passed to each unit test. *)
type testing_heaps = {
    small_heap : int fibheap;
};;

let tests = TestList
  (* Bracket each test by reinitialising test variables. *)
  (List.map
      (fun (name, test_fun) ->
        name >::
          (bracket
              (fun () ->
                let small_heap = fibheap_create () in
                  List.iter
                    (fun i -> ignore (fibheap_insert_data small_heap i i))
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

  assert_int_equal 1 (fibheap_extract_min_data heap);
(*   print_endline "** After first extract_min:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 2 (fibheap_extract_min_data heap);
(*   print_endline "** After second:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 3 (fibheap_extract_min_data heap);
(*   print_endline "** After third:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 4 (fibheap_extract_min_data heap);
(*   print_endline "** After fourth:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 5 (fibheap_extract_min_data heap);
(*   print_endline "** After fifth:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 6 (fibheap_extract_min_data heap);
(*   print_endline "** After sixth:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 7 (fibheap_extract_min_data heap);
(*   print_endline "** After seventh:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 8 (fibheap_extract_min_data heap);
(*   print_endline "** After eighth:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 9 (fibheap_extract_min_data heap);
(*   print_endline "** After ninth:"; flush stdout; *)
(*   print_heap heap; *)

  assert_int_equal 10 (fibheap_extract_min_data heap);
(*   print_endline "** After tenth:"; flush stdout; *)
(*   print_heap heap; *)

  try
    fibheap_extract_min_data heap;
    assert_failure ("heap not Empty")
  with IntMinHeap.Empty -> ();
);

"extract min small heap munge",
(fun {small_heap=heap} ->
  assert_int_equal 1 (fibheap_extract_min_data heap);
  assert_int_equal 2 (fibheap_extract_min_data heap);

  fibheap_insert_data heap 1 1;
  assert_int_equal 1 (fibheap_extract_min_data heap);
  assert_int_equal 3 (fibheap_extract_min_data heap);

  fibheap_insert_data heap 11 11;
  fibheap_insert_data heap 1 1;
  fibheap_insert_data heap 1 1;
  assert_int_equal 1 (fibheap_extract_min_data heap);
  assert_int_equal 1 (fibheap_extract_min_data heap);
  assert_int_equal 4 (fibheap_extract_min_data heap);
  assert_int_equal 5 (fibheap_extract_min_data heap);
  assert_int_equal 6 (fibheap_extract_min_data heap);
  assert_int_equal 7 (fibheap_extract_min_data heap);
  assert_int_equal 8 (fibheap_extract_min_data heap);
  assert_int_equal 9 (fibheap_extract_min_data heap);
  assert_int_equal 10 (fibheap_extract_min_data heap);
  assert_int_equal 11 (fibheap_extract_min_data heap);

  try
    fibheap_extract_min_data heap;
    assert_failure ("heap should be Empty")
  with IntMinHeap.Empty -> ();
);

"stress",
(fun _ ->
  let heap = fibheap_create () in
  let n = 10000 in
    for i = 0 to n - 1 do
      fibheap_insert_data heap i i;
    done;
    for i = 0 to n - 1 do
      assert_int_equal (n - i) (fibheap_size heap);
      assert_int_equal i (fibheap_extract_min_data heap);
    done;
);

"decrease key",
(fun _ ->
  let heap = fibheap_create () in
  let node10 = fibnode_new ~key:10 ~data:10 in
  let node20 = fibnode_new ~key:20 ~data:20 in
    fibheap_insert heap node10;
    fibheap_insert heap node20;
    fibheap_decrease_key heap node20 5;
    assert_int_equal 20 (fibheap_extract_min_data heap);
    assert_int_equal 10 (fibheap_extract_min_data heap);
    try
      ignore (fibheap_extract_min_data heap);
      assert_failure "heap not empty";
    with IntMinHeap.Empty -> ()
);

(* The following test isn't really random, though it does use a PRNG. If you
   change the seed, you will get new heap structure. But I leave the seed constant
   for reproducible results. *)
(*  *)
"random",
(fun _ ->
  let n = 1000 in
  let heap = fibheap_create () in
  let seed = 1024768 in
  let rstate = Random.State.make [| seed |] in

  (* List of nodes in the heap. *)
  let node_list =
    let rec genelts i =
      if i = 0 then []
      else
        let key = 1 + Random.State.int rstate (n - 1) in
        let node = fibheap_insert_data heap key key in
          assert (key > 0);
          node :: genelts (i - 1)
    in
      genelts n
  in
    assert (List.length node_list = n);
    List.iter
      (fun n ->
        fibheap_decrease_key heap n 0;
        assert (fibnode_key n = 0);
        let actual_min = fibheap_extract_min heap in
          assert_data_equal (fibnode_data n) (fibnode_data actual_min))
      node_list;

);

]);;

let rec run_raw_test test =
  match test with
  | TestCase f -> f ()
  | TestList ts -> List.iter run_raw_test ts
  | TestLabel (str, t) -> run_raw_test t;;

(* run_raw_test tests;; *)
run_test_tt ~verbose:true tests;;
