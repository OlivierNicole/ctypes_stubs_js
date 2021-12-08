open! Ctypes
let%expect_test _ =
  print_endline "hello";
  [%expect {| hello |}]
