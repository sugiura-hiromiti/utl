open OUnit2
open Tutorial

let tests =
  "test suites"
  >::: [
         ("empty" >:: fun _ -> assert_equal 0 0);
         ("sum" >:: fun _ -> assert_equal (Sum.sum [ 7; 7 ]) 14);
         ("rcrd" >:: fun _ -> assert_equal (Sum.rcrd "" 0) { a = ""; b = 0 });
         ("new_rcrd" >:: fun _ -> assert_equal Sum.new_rcrd { a = ""; b = 0 });
         ("unwrap" >:: fun _ -> assert_equal (Sum.unwrap (Some 0)) 0);
         ("list_max" >:: fun _ -> assert_equal (Sum.list_max []) None);
         ( "list_max2" >:: fun _ ->
           assert_equal (Sum.list_max [ 1; 2; 3 ]) (Some 3) );
         ( "list_of_intlist" >:: fun _ ->
           assert_equal
             (Sum.intlist_of_list [ 3; 2; 9 ])
             (Sum.Cons (3, Sum.Cons (2, Sum.Cons (9, Sum.Nil))))
             ~printer:Sum.string_of_intlist );
         ("intlist_length" >:: fun _ -> assert_equal 0 0);
       ]

let _ = run_test_tt_main tests
