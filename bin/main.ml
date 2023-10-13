open Printf

type action = None | Add | Get | Edit | Remove
(* type record = { login : string; password : string } *)

let default_filename = "passwords"

let () =
  let usage_msg =
    "Usage: " ^ Sys.argv.(0)
    ^ " <action> -l <login> [-p <password>]\n\
       action: add | edit | remove\n\
       arguments:"
  in
  let user_action = ref None in
  let login = ref "" in
  let password = ref "" in
  let anon_args arg =
    user_action :=
      match arg with
      | "add" -> Add
      | "get" -> Get
      | "edit" -> Edit
      | "remove" -> Remove
      | _ -> None
  in
  let args =
    [
      ("-l", Arg.Set_string login, "login");
      ("-p", Arg.Set_string password, "password");
    ]
  in
  Arg.parse (Arg.align args) anon_args usage_msg;

  let find record in_channel =
    let read_line () =
      try Some (input_line in_channel) with End_of_file -> None
    in
    let rec check_line = function
      (* TODO: add check to invalid argument in String.sub function  *)
      | Some line ->
          if String.length line < String.length record then
            check_line (read_line ())
          else if String.sub line 0 (String.length record) = record then
            Some line
          else check_line (read_line ())
      | None ->
          close_in in_channel;
          None
    in
    check_line (read_line ())
  in

  let do_action = function
    | Add ->
        let oc = Out_channel.open_gen [ Open_append ] 0o600 default_filename in
        fprintf oc "%s\n" (!login ^ ":" ^ !password);
        close_out oc
    | Get -> (
        let ic = In_channel.open_gen [ Open_rdonly ] 0o600 default_filename in
        match find !login ic with
        | Some l -> print_endline l
        | None ->
            ();
            flush stdout)
    | Edit -> ()
    | Remove -> ()
    | _ -> ()
  in
  do_action !user_action
