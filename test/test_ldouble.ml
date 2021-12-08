open! Ctypes
open! LDouble

let print d = to_float d |> Printf.printf "%f"

let floats = [0.; 1.; 2.**40.; -. (2.**40.); 1. /. 123. ]

let ts = List.map of_float floats

let%expect_test "roundtrip float" =
  List.iter (fun x ->
      let y = x |> of_float |> to_float in
      if y <> x
      then Printf.printf "%f doesn't roundtrip\n" x) floats;
  [%expect {||}]

let%expect_test "roundtrip int" =
  List.iter (fun x ->
      let y = x |> of_int |> to_int in
      if y <> x
      then Printf.printf "%d doesn't roundtrip\n" x) [-10; 0; 10; 1234567];
  [%expect {||}]


let%expect_test "to_string" =
  List.iter (fun x ->
      print_endline (to_string x)) ts;
  [%expect {|
    0.000000
    1.000000
    1099511627776.000000
    -1099511627776.000000
    0.008130 |}]


let%expect_test "of_string" =
  List.iter (fun x ->
      let s = Printf.sprintf "%.20f" x in
      let t = of_string s in
      let x' = float_of_string s in
      if t <> of_float x'
      then
        Printf.printf "of_string(%s) = %s <> %.20f, \n" s (to_string ~width:20 ~prec:20 t) x') floats;
  [%expect {| |}]



