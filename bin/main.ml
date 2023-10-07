open Printf

let () =
  let usage_msg =
    "Usage: keeper <action> -l <login> [-p <password>]\n\
     action: add | edit | remove\n\
     arguments:"
  in
  let action = ref "" in
  let login = ref "" in
  let password = ref "" in
  let anon param = if !action == "" then action := param in
  let args =
    [
      ("-l", Arg.Set_string login, "login");
      ("-p", Arg.Set_string password, "password");
    ]
  in
  Arg.parse (Arg.align args) anon usage_msg;
  printf "actions: %s\n" !action;
  printf "login: %s, password: %s\n" !login !password
