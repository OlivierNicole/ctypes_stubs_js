open! Ctypes
open! ComplexL

let platform_dependent = true

let double_str d =
  match LDouble.classify d with
  | FP_nan -> Printf.sprintf "nan"
  | _ -> LDouble.to_string ~prec:6 d

let str d =
  let r = re d in
  let i = im d in
  match (LDouble.classify r, LDouble.classify i) with
  | (FP_nan, _) | (_, FP_nan) -> "nan"
  | (FP_zero, FP_zero) -> LDouble.to_string ~prec:6 LDouble.zero
  | (_, FP_zero) -> LDouble.to_string ~prec:6 r
  | (FP_zero, _) -> LDouble.to_string ~prec:6 i ^ "i"
  | _ ->
      Printf.sprintf
        "%s + %si"
        (LDouble.to_string ~prec:6 r)
        (LDouble.to_string ~prec:6 i)

let print d = Printf.printf "%s" (str d)

let ts =
  List.map
    (fun (r, i) -> make (LDouble.of_int r) (LDouble.of_int i))
    [(0, 0); (1, 0); (0, 1); (1, 1); (1, -1); (-1, 1); (-1, 1)]

let ts_small = ts

let test_bin ?(data = ts) bin =
  List.iter
    (fun x ->
      List.iter
        (fun y ->
          Printf.printf "op(%s,%s) = %s\n" (str x) (str y) (str (bin x y)))
        data)
    data

let test_un un =
  List.iter (fun x -> Printf.printf "op(%s) = %s\n" (str x) (str (un x))) ts

let print_digest ?(verbose = false) s =
  if verbose then print_endline s ;
  print_endline (Digest.to_hex (Digest.string s))

let unimplemented () = print_endline "UNIMPLEMENTED"

let%expect_test "neg" =
  test_un neg ;
  print_digest [%expect.output] ;
  [%expect {|
    5871084894b7b8f2c484e9f718911cbd |}]

let%expect_test "conj" =
  test_un conj ;
  print_digest [%expect.output] ;
  [%expect {|
    345af6a384af686bacb108d07ef96d98 |}]

let%expect_test "add" =
  test_bin add ;
  print_digest [%expect.output] ;
  [%expect {|
    fb1894b5abdb2f6ce95918fa38c8c37c |}]

let%expect_test "sub" =
  test_bin sub ;
  print_digest [%expect.output] ;
  [%expect {|
    07c97028778f7c9fa7c612524f9ca1fa |}]

let%expect_test "mul" =
  test_bin mul ;
  print_digest [%expect.output] ;
  [%expect {|
    52101eeb47372c180346ae619894dc94 |}]

let%expect_test "div" =
  test_bin div ;
  print_digest [%expect.output] ;
  [%expect
    {|
    797d962017a927c35e918bee9f3e487f |}]

let%expect_test "inv" =
  test_un inv ;
  print_digest [%expect.output] ;
  [%expect {|
    6c45a872d2d7e28e61540352d87cecd2 |}]

let%expect_test "sqrt" =
  if true then unimplemented () else test_un sqrt ;
  print_digest [%expect.output] ;
  [%expect {|
    018977842f417a5173dee5d97ea0efe4 |}]

let%expect_test "exp" =
  if true then unimplemented () else test_un exp ;
  print_digest [%expect.output] ;
  [%expect {|
    018977842f417a5173dee5d97ea0efe4 |}]

let%expect_test "log" =
  if true then unimplemented () else test_un log ;
  print_digest [%expect.output] ;
  [%expect {|
    018977842f417a5173dee5d97ea0efe4 |}]

let%expect_test "pow" =
  if true then unimplemented () else test_bin pow ;
  print_digest [%expect.output] ;
  [%expect {|
    018977842f417a5173dee5d97ea0efe4 |}]

let%expect_test "norm" =
  List.iter
    (fun x -> Printf.printf "op(%s) = %s\n" (str x) (double_str (norm x)))
    ts ;
  print_digest [%expect.output] ;
  [%expect {|
    6d25604983d051e3af704e17ec963de6 |}]

let%expect_test "norm2" =
  List.iter
    (fun x -> Printf.printf "op(%s) = %s\n" (str x) (double_str (norm2 x)))
    ts ;
  print_digest [%expect.output] ;
  [%expect {|
    f29e518055f7ea757b575adf3eefbd0f |}]

let%expect_test "arg" =
  if true then unimplemented ()
  else
    List.iter
      (fun x -> Printf.printf "op(%s) = %s\n" (str x) (double_str (arg x)))
      ts ;
  print_digest [%expect.output] ;
  [%expect {|
    018977842f417a5173dee5d97ea0efe4 |}]

let%expect_test "polar" =
  List.iter
    (fun (x, y) ->
      Printf.printf
        "op(%s,%s) = %s\n"
        (double_str x)
        (double_str y)
        (str (polar x y)))
    [
      (LDouble.zero, LDouble.zero);
      (LDouble.zero, LDouble.one);
      (LDouble.one, LDouble.one);
    ] ;
  print_digest [%expect.output] ;
  [%expect {|
    8fbe2355e1c9eb5286767743661ea7f7 |}]

let%expect_test "equal" =
  List.iter
    (fun x ->
      List.iter
        (fun y -> Printf.printf "equal(%s,%s) = %b\n" (str x) (str y) (x = y))
        ts)
    ts ;
  print_digest [%expect.output] ;
  [%expect
    {|
    8cc546c98b7de7c32524ce8327bc645d |}]

let%expect_test "compare" =
  List.iter
    (fun x ->
      List.iter
        (fun y ->
          Printf.printf "compare(%s,%s) = %d\n" (str x) (str y) (compare x y))
        ts)
    ts ;
  print_digest [%expect.output] ;
  [%expect
    {|
    d1eb110aaf27c2dcea84a4a80704fb3d |}]
