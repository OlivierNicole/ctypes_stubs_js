open! Ctypes
open! LDouble

let platform_dependent = false

let str d =
  match classify d with
  | FP_nan -> Printf.sprintf "nan"
  | _ -> to_string ~prec:6 d

let print d = Printf.printf "%s" (str d)

let floats =
  [0.; 1.; 2. ** 40.; -.(2. ** 40.); 1. /. 123.; 2.220446049250313e-16]

let ts = List.map of_float floats

let ts_small = List.map of_float [-213.; 123123.; 0.; 1.; 34.43]

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

let%expect_test "roundtrip float" =
  List.iter
    (fun x ->
      let y = x |> of_float |> to_float in
      if y <> x then Printf.printf "%f doesn't roundtrip\n" x)
    floats ;
  [%expect {||}]

let%expect_test "roundtrip int" =
  List.iter
    (fun x ->
      let y = x |> of_int |> to_int in
      if y <> x then Printf.printf "%d doesn't roundtrip\n" x)
    [-10; 0; 10; 1234567] ;
  [%expect {||}]

let%expect_test "to_string" =
  List.iter
    (fun (width, prec) ->
      List.iter (fun x -> print_endline (to_string ?width ?prec x)) ts)
    [(None, None); (Some 2, Some 2)] ;
  [%expect
    {|
    0.000000
    1.000000
    1099511627776.000000
    -1099511627776.000000
    0.008130
    0.000000
    0.00
    1.00
    1099511627776.00
    -1099511627776.00
    0.01
    0.00 |}]

let%expect_test "of_string" =
  List.iter
    (fun x ->
      let s = Printf.sprintf "%.40f" x in
      let t = of_string s in
      let x' = float_of_string s in
      if t <> of_float x' then
        Printf.printf
          "of_string(%s) = %s <> %.40f, \n"
          s
          (to_string ~width:20 ~prec:40 t)
          x')
    floats ;
  [%expect {| |}]

let%expect_test "add" =
  test_bin ~data:ts_small add ;
  print_digest [%expect.output] ;
  [%expect {|
    fd0517a8522089f89ea71929b2c2ddb1 |}]

let%expect_test "sub" =
  test_bin ~data:ts_small sub ;
  print_digest [%expect.output] ;
  [%expect {|
    3554323f9b80d3ccb941408611b8b1e1 |}]

let%expect_test "mul" =
  test_bin ~data:ts_small sub ;
  print_digest [%expect.output] ;
  [%expect {|
    3554323f9b80d3ccb941408611b8b1e1 |}]

let%expect_test "div" =
  test_bin ~data:ts_small sub ;
  print_digest [%expect.output] ;
  [%expect {|
    3554323f9b80d3ccb941408611b8b1e1 |}]

let%expect_test "neg" =
  test_un neg ;
  print_digest [%expect.output] ;
  [%expect {|
    b9173c6ec5f3b8451d4ba002edcb907f |}]

let%expect_test "pow" =
  test_bin pow ;
  print_digest [%expect.output] ;
  [%expect {|
    bb7f47c085504b1904b27ef8acd88770 |}]

let%expect_test "sqrt" =
  test_un sqrt ;
  print_digest [%expect.output] ;
  [%expect {|
    0779d91b27865e5b69ea42f7cfc65102 |}]

let%expect_test "exp" =
  test_un exp ;
  print_digest [%expect.output] ;
  [%expect {|
    7d2ab01e1358859206d2264b9b65e52c |}]

let%expect_test "log" =
  test_un log ;
  print_digest [%expect.output] ;
  [%expect {|
    951254e429fcdc13c8aa36cdd108852f |}]

let%expect_test "log10" =
  test_un log10 ;
  print_digest [%expect.output] ;
  [%expect {|
    18fc28a473497e36074c9d2eb29bfdf3 |}]

let%expect_test "expm1" =
  test_un expm1 ;
  print_digest [%expect.output] ;
  [%expect {|
    04230e716cde3ef40b0e4c7cb085e38b |}]

let%expect_test "log1p" =
  test_un log1p ;
  print_digest [%expect.output] ;
  [%expect {|
    f082b2cf4569ffbaf7b054a0b59aff59 |}]

let%expect_test "cos" =
  test_un cos ;
  print_digest [%expect.output] ;
  [%expect {|
    1154e8059f0c81fa8a793c0b267d6a91 |}]

let%expect_test "sin" =
  test_un sin ;
  print_digest [%expect.output] ;
  [%expect {|
    693dbd1157d474d4ad15b5df9e3757e8 |}]

let%expect_test "tan" =
  test_un tan ;
  print_digest [%expect.output] ;
  [%expect {|
    f0eda39c91c991b57162a64bf51a0447 |}]

let%expect_test "acos" =
  test_un acos ;
  print_digest [%expect.output] ;
  [%expect {|
    59dc3326f19bd28f0d66993fee77fc45 |}]

let%expect_test "asin" =
  test_un asin ;
  print_digest [%expect.output] ;
  [%expect {|
    a68d503156dc9fcc31fdbbed3a48b6d7 |}]

let%expect_test "atan" =
  test_un atan ;
  print_digest [%expect.output] ;
  [%expect {|
    f4dddfffe692770e81f1aa444a7e039e |}]

let%expect_test "atan2" =
  test_bin atan2 ;
  print_digest [%expect.output] ;
  [%expect {|
    2311db34e993218fc72cd7f6236968ae |}]

let%expect_test "hypot" =
  test_bin ~data:ts_small hypot ;
  print_digest [%expect.output] ;
  [%expect {|
    a90a011f2a28f8ae4f1cb49013e0b45c |}]

let%expect_test "cosh" =
  test_un cosh ;
  print_digest [%expect.output] ;
  [%expect {|
    04f1663cbeef72e402eeff25250dd43f |}]

let%expect_test "sinh" =
  test_un sinh ;
  print_digest [%expect.output] ;
  [%expect {|
    459b153d820a811c6f732cb7abc2e387 |}]

let%expect_test "tanh" =
  test_un tanh ;
  print_digest [%expect.output] ;
  [%expect {|
    7da485844d7d8eece1400a0c9264f203 |}]

let%expect_test "acosh" =
  test_un acosh ;
  print_digest [%expect.output] ;
  [%expect {|
    8091063563e4b7d64ae59c1cb066c0c7 |}]

let%expect_test "asinh" =
  test_un asinh ;
  print_digest [%expect.output] ;
  [%expect {|
    a0c9718f41c777d866b1f329a5bd6b25 |}]

let%expect_test "atanh" =
  test_un atanh ;
  print_digest [%expect.output] ;
  [%expect {|
    9bddfd06d54034ad7249cc664b5a6472 |}]

let%expect_test "ceil" =
  test_un ceil ;
  print_digest [%expect.output] ;
  [%expect {|
    d3c602fbd6fee575a4c5080b5274a09d |}]

let%expect_test "floor" =
  test_un floor ;
  print_digest [%expect.output] ;
  [%expect {|
    aa0d6058df2524e49934f8cda65b40d8 |}]

let%expect_test "abs" =
  test_un abs ;
  print_digest [%expect.output] ;
  [%expect {|
    1cbf574dd06e7c3e0bb43623c51af1a9 |}]

(* let%expect_test "rem" =
 *   test_bin rem;
 *   print_digest [%expect.output];
 *   [%expect {|
 *     2f4e67cb1001c20fca8fc4e8ba912fd9 |}] *)

let%expect_test "copysign" =
  test_bin copysign ;
  print_digest [%expect.output] ;
  [%expect {|
    50970723f7f7019182b7c892b268badb |}]

let%expect_test "frexp" =
  unimplemented () ;
  [%expect {|
    UNIMPLEMENTED |}]

let%expect_test "ldexp" =
  unimplemented () ;
  [%expect {|
    UNIMPLEMENTED |}]

let%expect_test "modf" =
  unimplemented () ;
  [%expect {|
    UNIMPLEMENTED |}]

let%expect_test "classify" =
  List.iter
    (fun x ->
      Printf.printf
        "%s => %s\n"
        (str x)
        (match classify x with
        | FP_nan -> "nan"
        | FP_infinite -> "infinite"
        | FP_normal -> "normal"
        | FP_subnormal -> "subnormal"
        | FP_zero -> "zero"))
    ts ;
  [%expect
    {|
    0.000000 => zero
    1.000000 => normal
    1099511627776.000000 => normal
    -1099511627776.000000 => normal
    0.008130 => normal
    0.000000 => normal |}]

let%expect_test "constant" =
  if platform_dependent then print min_float ;
  [%expect {| |}] ;
  if platform_dependent then print max_float ;
  [%expect {| |}] ;
  print epsilon ;
  [%expect {| 0.000000 |}] ;
  print nan ;
  [%expect {| nan |}] ;
  print infinity ;
  [%expect {| inf |}] ;
  print neg_infinity ;
  [%expect {| -inf |}] ;
  print zero ;
  [%expect {| 0.000000 |}] ;
  print one ;
  [%expect {| 1.000000 |}] ;
  if platform_dependent then
    Printf.printf "%d - %d" (fst byte_sizes) (snd byte_sizes) ;
  [%expect {| |}] ;
  if platform_dependent then Printf.printf "%d" mant_dig ;
  [%expect {| |}]

let%expect_test "equal" =
  List.iter
    (fun x ->
      List.iter
        (fun y -> Printf.printf "equal(%s,%s) = %b\n" (str x) (str y) (x = y))
        ts)
    ts ;
  print_digest [%expect.output] ;
  [%expect {|
    f3e5bd8b452e5939c60b18ab5d314e9d |}]

let%expect_test "compare" =
  List.iter
    (fun x ->
      List.iter
        (fun y ->
          Printf.printf "compare(%s,%s) = %d\n" (str x) (str y) (compare x y))
        ts)
    ts ;
  print_digest [%expect.output] ;
  [%expect {|
    c13034eac5008473a07b646aa0b228d9 |}]
