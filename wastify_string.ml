open StdLabels

let input =
   if Array.length Sys.argv = 2 then
      In_channel.open_text Sys.argv.(1)
   else if Array.length Sys.argv = 1 then
      stdin
   else
      failwith @@ Format.sprintf
         "usage: %s <input file> | %s (input from stdin)"
         Sys.argv.(0)
         Sys.argv.(0)

let parse_line l =
   let i_space = String.index l ' ' in
   assert (not (Int.equal i_space 0));
   let name = String.sub l ~pos:0 ~len:i_space in
   let rem =
      String.sub l ~pos:(i_space + 1) ~len:(String.length l - i_space - 1)
   in
   name, rem

let hexify fmt str =
   let open Format in
   String.iteri str ~f:(fun i char ->
      fprintf fmt "(i32.const@ 0x%x)" (Char.code char);
      if not (Int.equal i (String.length str - 1)) then fprintf fmt "@ ")

let () =
   let entries =
      In_channel.fold_lines
         (fun entries l -> parse_line  l :: entries) [] input
      |> List.rev
   in
   let open Format in
   printf "@[<v>";
   List.iter entries ~f:(fun (name, str) ->
      printf
         "@[<v>;; \"%s\"@,@[<v 3>\
          (global $%s_ERRMSG (export \"%s_ERRMSG\")@,\
          (ref $string) (array.new_fixed $string@,@[<hov>"
         str
         (String.uppercase_ascii name)
         (String.uppercase_ascii name);
      hexify std_formatter str;
      printf "))@]@]@,@,@]");
   printf "@,@,";
   printf ";;;;;;;;;;@,@,";
   List.iter entries ~f:(fun (name, _) ->
      printf "@[<hov 3>(import@ \"env\"@ \"%s_ERRMSG\"@ \
              @[<hov 3>(global@ $%s_ERRMSG@ (ref@ $string)))@]@]@,"
         (String.uppercase_ascii name)
         (String.uppercase_ascii name))
