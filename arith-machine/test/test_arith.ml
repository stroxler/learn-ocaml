open Base
open OUnit2
open Arith


let tests = [
  "Arith.simple_run works as expected" >::
  (fun _ ->
   let actual = Arith.simple_run (
       (3 |*| 5) -+- ((1 |+| 2) -*- (4 |+| -3))
     ) in
   assert_equal actual 18L ~printer:Int64.to_string
  );
  "Arith.cps_run works as expected" >::
  (fun _ ->
   let actual = Arith.cps_run (
       (3 |*| 5) -+- ((1 |+| 2) -*- (4 |+| -3))
     ) in
   assert_equal actual 18L ~printer:Int64.to_string
  );
  "Arith.stack_run works as expected" >::
  (fun _ ->
   let actual = Arith.stack_run (
       (3 |*| 5) -+- ((1 |+| 2) -*- (4 |+| -3))
     ) in
   assert_equal actual 18L ~printer:Int64.to_string
  );
  (* TEMPORARY *)
  "product works " >::
  (fun _ ->
     assert_equal (Arith.product []) 1;
     assert_equal (Arith.product [(-3)]) (-3);
     assert_equal (Arith.product [4; 5]) 20;
  );
  "concat_strings works" >::
  (fun _ ->
     assert_equal (Arith.concat_strings []) "";
     assert_equal (Arith.concat_strings ["this"; "that"]) "thisthat"
  );
  "add works " >::
  (fun _ ->
     assert_equal (Arith.add 2 3) 5 ~printer:Int.to_string;
     assert_equal (Arith.add (-5) 3) (-2) ~printer:Int.to_string
  );
  "third works " >::
  (fun _ ->
     assert_equal (Arith.third_or_0 [1; 2]) 0 ~printer:Int.to_string;
     assert_equal (Arith.third_or_0 [1; 2; 3; 4;]) 3 ~printer:Int.to_string;
  );
  "powerset" >::
  (fun _ ->
     assert_equal (powerset []) [[]];
     assert_equal (powerset [1]) [[]; [1]];
     assert_equal (powerset [1; 2]) [[]; [1]; [2]; [1; 2]];
  );
  "same_shape" >::
  (fun _ ->
     assert_equal false @@ same_shape
       Leaf
       (Node (1, Leaf, Leaf));
     assert_equal true @@ same_shape
       (Node ( 3 , (Node ( 5 , Leaf, Leaf)), Leaf))
       (Node ("3", (Node ("5", Leaf, Leaf)), Leaf));
  );
  "is_bst" >::
  (fun _ ->
     assert_equal false @@ is_bst
       (Node (5, (Node (3, Leaf, Leaf)), (Node (4, Leaf, Leaf))));
     assert_equal false @@ is_bst
       (Node (5, (Node (6, Leaf, Leaf)), (Node (7, Leaf, Leaf))));
     assert_equal true @@ is_bst
       (Node (5, (Node (3, Leaf, Leaf)), (Node (7, Leaf, Leaf))));
  );
  "intercalate" >::
  (fun _ ->
     assert_equal
       "this,that" ~printer:(fun x -> x)
       (intercalate "," ["this"; "that"])
  );
  "add_row_vectors" >::
  (fun _ ->
     assert_equal
       [5; 7; 9]
       (add_row_vectors [1 ; 2; 3] [4; 5; 6])
  );
  "is_valid_matrix" >::
  (fun _ ->
     assert_equal
       false @@
       is_valid_matrix [];
     assert_equal
       false @@
       is_valid_matrix [[]; []];
     assert_equal
       false @@
       is_valid_matrix [[1; 2]; [3; 4; 5]];
     assert_equal
       true @@
       is_valid_matrix [[1; 2]; [3; 4]];
  );
]
