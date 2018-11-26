open OUnit2
open Matrix
open Vector
open Array
open Complex

let list1 = [5.0;4.0;3.0;2.0;1.0]
let list2 = [10.0;8.0;6.0;4.0;2.0]
let list3 = [4.0;3.0]
let list4 = [0.0;0.0;0.0]
let list5 = [1.0;2.0]
let list6 = [3.0;4.0]
let list7 = [4.0;6.0]
let list8 = [1.0;2.0;3.0]
let list9 = [4.0;5.0;6.0]
let list10 = [7.0;8.0;9.0]
let list11 = [1.0;4.0;7.0]
let list12 = [2.0;5.0;8.0]
let list13 = [3.0;6.0;9.0]
let list14 = [2.0;2.0]
let list15 = [10.0;10.0]
let list16 = [2.0;3.0]
let list17 = [4.0;5.0]
let list18 = [10.0;13.0]
let list19 = [22.0;29.0]
let vector1 = Vector.empty
let vector2 = cons_v 1.0 vector1
let vector3 = cons_v 2.0 vector2
let vector4 = cons_v 3.0 vector3
let vector5 = cons_v 4.0 vector4
let vector6 = cons_v 5.0 vector5
let vector7 = make_v list1
let vector8 = cons_v 1.5 vector1
let vector9 = cons_v 2.5 vector1
let vector10 = make_v list2
let vector11 = make_v list3
let vector12 = make_v list4
let vector13 = make_v list5
let vector14 = make_v list6
let vector15 = make_v list7
let v1 = make_v list8
let v2 = make_v list9
let v3 = make_v list10
let t1 = make_v list11
let t2 = make_v list12
let t3 = make_v list13
let vector16 = make_v list14
let vector17 = make_v list15


let vector_tests = [

  "test1">::
  (fun _ -> assert_equal(true)(is_empty vector1));

  "test2" >::
  (fun _ -> assert_equal(false)(is_empty vector2));

  "test3" >::
  (fun _ -> assert_equal(vector3)(cons_v 2.0 vector2));

  "test4" >::
  (fun _ -> assert_equal(vector6)(vector7));

  "test5" >:: 
  (fun _ -> assert_equal (0)(dim_v vector1));

  "test6" >:: 
  (fun _ -> assert_equal (1)(dim_v vector2));

  "test7" >::
  (fun _ -> assert_equal (vector9)(add_v vector2 vector8));

  "test8" >::
  (fun _ -> assert_raises(Invalid_argument "List.map2")(fun () -> add_v vector2 vector5));

  "test9" >::
  (fun _ -> assert_equal (vector10)(scale_v 2.0 vector7));

  "test10" >::
  (fun _ -> assert_equal (vector7)(sub_v vector10 vector7));

  "test11" >::
  (fun _ -> assert_raises(Invalid_argument "List.map2")(fun () -> sub_v vector10 vector3));

  "test12" >::
  (fun _ -> assert_equal(110.0)(dot_v vector7 vector10));

  "test13" >::
  (fun _ -> assert_equal(5.0)(mag_v vector11));

  "test14" >::
  (fun _ -> assert_equal(vector12)(zero_vector 3));

]

let matrix = Matrix.empty
let matrix1 = add_row vector12 matrix
let matrix2 = add_row vector12 matrix1
let matrix3 = add_row vector12 matrix2
let matrix4 = add_row vector11 matrix
let matrix5 = add_row vector11 matrix4
let matrix6 = add_row vector14 matrix |> add_row vector13
let matrix7 = add_row vector14 matrix
let matrix8 = add_row vector13 matrix
let matrix9 = add_row vector15 matrix
let matrix10 = add_row v1 (add_row v2 (add_row v3 matrix))
let matrix11 = add_row t1 (add_row t2 (add_row t3 matrix))
let matrix10_T = [[1.0;4.0;7.0];[2.0;5.0;8.0];[3.0;6.0;9.0]]
let matrix11_T = [[1.0;2.0;3.0];[4.0;5.0;6.0];[7.0;8.0;9.0]]
let matrix12 = add_row vector16 matrix
let matrix13 = add_row vector17 matrix
let matrix14 = add_row (make_v list16) (add_row (make_v list17) matrix)
let matrix15 = add_row (make_v list18) (add_row (make_v list19) matrix)
let matrix16 = add_row (make_v list5) (add_row (make_v list6) matrix)
let matrix17 = add_row t3 (add_row v2 (add_row v3 matrix))

let array1 = [|[|1.0;2.0;3.0|];[|4.0;5.0;6.0|];[|7.0;8.0;9.0|]|]
let array2 = [|3.0;6.0;9.0|]
let array3 = [|[|1.0;2.0;3.0|];[|3.0;6.0;9.0|];[|7.0;8.0;9.0|]|]
let array4 = [|[|1.0;2.0;-1.0;-4.0|];[|2.0;3.0;-1.0;-11.0|];[|-2.0;0.0;-3.0;22.0|]|]
let list5 = [[1.0;0.0;0.0;-8.0];[0.0;1.0;0.0;1.0];[0.0;0.0;1.0;-2.0]]

let matrix_tests = [

  "testOne" >::
  (fun _ -> assert_equal(matrix3)(zero_matrix 3 3));

  "testTwo" >::
  (fun _ -> assert_equal (0)(dim_hd matrix));

  "testThree" >::
  (fun _ -> assert_equal (2)(dim_hd matrix4));

  "testFour" >::
  (fun _ -> assert_equal ((2,2))(dim matrix5));

  "testFive" >::
  (fun _ -> assert_equal ((1,2))(dim matrix4));

  "testSix" >::
  (fun _ -> assert_equal ((3,3))(dim matrix3));

  "testSeven" >::
  (fun _ -> assert_equal (true)(is_square matrix3));

  "testEight" >::
  (fun _ -> assert_equal (false)(is_square matrix4));


  "testNine" >::
  (fun _ -> assert_equal (1.0)(elem 1 1 matrix6));

  "testTen" >::
  (fun _ -> assert_equal (2.0)(elem 1 2 matrix6));

  "testEleven" >::
  (fun _ -> assert_equal (3.0)(elem 2 1 matrix6));

  "testTwelve" >::
  (fun _ -> assert_equal (4.0)(elem 2 2 matrix6));


  "testThirteen" >::
  (fun _ -> assert_equal (matrix9)(Matrix.add matrix8 matrix7));

  "testFourteen" >::
  (fun _ -> assert_raises(DimUnequal)(fun () -> (Matrix.add matrix2 matrix8)));

  "testSeventeen" >::
  (fun _ -> assert_equal(matrix12)(Matrix.sub matrix7 matrix8));

  "testEighteen" >::
  (fun _ -> assert_raises(DimUnequal)(fun () -> (Matrix.sub matrix2 matrix8)));

  "testNineteen" >::
  (fun _ -> assert_equal(matrix13)(scale 5.0 matrix12));

  "testTwenty" >::
  (fun _ -> assert_equal(matrix10_T)(to_lst (trans matrix10)));

  "testTwenty" >::
  (fun _ -> assert_equal(matrix11_T)(to_lst (trans matrix11)));

  "testTwentyOne" >::
  (fun _ -> assert_equal(matrix15)(mult matrix16 matrix14));


]
let v1 = make_v [1.0;2.0]
let v2 = make_v [3.0;4.0]
let v3 = make_v [1.0;0.0]
let v4 = make_v [0.0;1.0]
let v5 = make_v [2.;3.;4.]
let v6 = make_v [4.;5.;6.]
let v7 = make_v [7.;8.;10.]
let v8 = make_v [-1.;-1.;1.]
let v9 = make_v [-1.;4.;-2.]
let v10 = make_v [1.5;-2.5;1.]
let matrix101 = add_row v3 (add_row v4 matrix)
let matrix100 = add_row v1 (add_row v2 matrix) 
let matrix100rev = Matrix.rev matrix100
let array100 = [|[|1.0;0.0|];[|0.0;1.0|]|]
let list2 = Matrix.rev (Matrix.add_row (Vector.make_v [1.5;-0.5])  (Matrix.add_row (Vector.make_v [-2.0;1.0]) Matrix.empty))
let array2 = [|[|-2.0;1.0|];[|1.5;-0.5|]|]
let array3 = [|[|1.0;0.0;0.0|];[|0.0;1.0;0.0|];[|0.0;0.0;1.0|]|]
let array4 = [|[|-1.6;0.6|];[|1.4;-0.4|]|]
let mat2 = array_to_t array2
let mat3 = add_row (v1) (add_row v1 Matrix.empty)
let mat4 = add_row v5 (add_row v6 (add_row v7 Matrix.empty))
let mat5 = add_row v8 (add_row v9 (add_row v10 Matrix.empty))
let mat6 = add_row (make_v [2.0;3.0]) (add_row (make_v [7.0;8.0]) Matrix.empty)
let mat7 = add_row (make_v [-1.6;0.6])(add_row (make_v [1.4;-0.4]) Matrix.empty)

let matrix_tests2 = [

  "testI" >::
  (fun _ -> assert_equal (mat2)((inverse matrix100)));

  "testII" >::
  (fun _ -> assert_equal (true)(check_identity array100));

  "testIII" >::
  (fun _ -> assert_raises (NotInvertible) (fun () -> inverse mat3)) ;

  "testIV" >::
  (fun _ -> assert_equal (mat5)((inverse mat4)));

  "testV" >::
  (fun _ -> assert_equal(array_to_t array3)(rref_t mat4));

]


let v1 = [2.0;3.0]
let v2 = [1.0;7.0]

let v3 = [2.0;2.0;3.0]
let v4 = [0.0;3.0;0.0]
let v5 = [0.0;0.0;1.0]
let v6 = [0.0;0.0;0.0]

let v7 = [1.0;2.0;3.0]
let v8 = [4.0;5.0;6.0]
let v9 = [7.0;8.0;9.0]

let v10 = [12.0;3.0]
let v11 = [0.0;7.0]

let v12 = [1.0;4.0;7.0]
let v13 = [2.0;5.0;8.0]

let v14 = [2.0;0.0;0.0]
let v15 = [2.0;3.0;0.0]
let v16 = [3.0;0.0;1.0]

let v17 = [1.0;2.0;3.0;4.0] 
let v18 = [5.0;6.0;7.0;8.0]
let v19 = [9.0;10.0;11.0;12.0]
let v20 = [13.0;14.0;15.0;16.0]

let v21 = [1.0;2.0;3.0;4.0] 
let v22 = [0.0;6.0;7.0;8.0]
let v23 = [0.0;0.0;11.0;12.0]
let v24 = [0.0;0.0;0.0;16.0]

let matrixTest1 = add_row (make_v v1) (add_row (make_v v2) Matrix.empty)
let matrixTest2 = add_row (make_v v3) ((add_row (make_v v4) (add_row (make_v v5) Matrix.empty)))
let matrixTest3 = add_row (make_v v3) ((add_row (make_v v4) (add_row (make_v v6) Matrix.empty)))
let matrixTest4 = add_row (make_v v7) ((add_row (make_v v8) (add_row (make_v v9) Matrix.empty)))
let matrixTest5 = add_row (make_v v10) (add_row (make_v v11) Matrix.empty)
let matrixTest6 = add_row (make_v v7) (add_row (make_v v8) Matrix.empty)
let matrixTest7 = add_row (make_v v12) (add_row (make_v v13) Matrix.empty)
let matrixTest8 = add_row (make_v v14) ((add_row (make_v v15) (add_row (make_v v16) Matrix.empty)))
let matrixTest9 = add_row (make_v v17) (add_row (make_v v18) ((add_row (make_v v19) (add_row (make_v v20) Matrix.empty))))
let matrixTest10 = add_row (make_v v21) (add_row (make_v v22) ((add_row (make_v v23) (add_row (make_v v24) Matrix.empty))))
let matrixTest11 = add_row (make_v v21) (add_row (make_v v22) ((add_row (make_v v23) Matrix.empty)))

let eigenTest = ({re = 7.0;im = 0.0},{re = 12.0;im = 0.0})

let matrix_tests_final = [

  "testUno" >::
  (fun _ -> assert_equal(11.0)(determinant_two matrixTest1));

  "testDos" >::
  (fun _ -> assert_equal(6.0)(determinant_triangular matrixTest2));

  "testTres" >::
  (fun _ -> assert_equal(0.0)(determinant_triangular matrixTest3));

  "testTres" >::
  (fun _ -> assert_equal(0.0)(determinant_three matrixTest4));

  "testCinco" >::
  (fun _ -> assert_equal (matrixTest2)(row_space matrixTest2));

  "testSeis" >::
  (fun _ -> assert_equal (matrixTest6)(row_space matrixTest4));

  "testSiete" >::
  (fun _ -> assert_equal (matrixTest7)(col_space matrixTest4));

  "testOcho" >::
  (fun _ -> assert_equal(matrixTest8)(col_space matrixTest2));

  "testNueve" >::
  (fun _ -> assert_equal (true)(is_lower_triangular matrixTest2));

  "testNueve" >::
  (fun _ -> assert_equal (true)(is_upper_triangular matrixTest8));

  "testDiez" >::
  (fun _ -> assert_equal (false)(is_upper_triangular matrixTest2));

  "testOnce" >::
  (fun _ -> assert_equal (false)(is_lower_triangular matrixTest8));

  "testDoce" >::
  (fun _ -> assert_raises (NotTriangular)(fun () -> determinant_main matrixTest9));

  "testTrece" >::
  (fun _ -> assert_equal(1056.0)(determinant_main matrixTest10));

  "testCatorce" >::
  (fun _ -> assert_equal(6.0)(trace matrixTest2));

  "testQuince" >::
  (fun _ -> assert_equal (3)(rank matrixTest2));

  "testDieciseis" >::
  (fun _ -> assert_equal (`NoSol)(num_sols matrixTest2));

  "testDiecisiete" >::
  (fun _ -> assert_equal (`UniqueSol)(num_sols matrixTest3));

  "testDiecisiete" >::
  (fun _ -> assert_equal (`UniqueSol)(num_sols matrixTest11));


]

let suite = 
  "Lin-Alg test suite" >::: List.flatten [

    vector_tests;
    matrix_tests;
    matrix_tests2;
    matrix_tests_final;

  ]

let _ = run_test_tt_main suite