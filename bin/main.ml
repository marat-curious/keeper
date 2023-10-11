open Printf

type action = None | Add | Edit | Remove
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

  let do_action = function
    | Add ->
        let oc = Out_channel.open_gen [Open_append] 0o600 default_filename in
        fprintf oc "%s\n" (!login ^ ":" ^ !password);
        close_out oc
    | _ -> ()
  in
  do_action !user_action
