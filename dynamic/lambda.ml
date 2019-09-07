open Value

let pervasives = [
  "add", Abs ("x", Abs("y", Native (fun env -> Lit (int_of_t env "x" + int_of_t env "y"))));
  "gt", Abs ("x", Abs("y", Native (fun env -> Lit (if int_of_t env "x" > int_of_t env "y" then 1 else 0))));
  "if", Abs ("c", Abs("t", Abs("f", Native (fun env -> get env (if int_of_t env "c" = 0 then "f" else "t")))));
  "print", Abs ("x", Native (fun env -> print_endline @@ string_of_t @@ get env "x"; Lit 0))
]

let test_eval () =
  [
    Lit 2, Lit 2;
    App (Abs ("x", Id "x"), Lit 2), Lit 2;
    App (App (Id "add", Lit 2), Lit 2), Lit 4;
    App (App (Id "gt", Lit 1), Lit 2), Lit 0;
    App (App (App (Id "if", Lit 0), Lit 2), Lit 3), Lit 3;
    let cond = App (App (Id "gt", Lit 3), Lit 1) in
      App (App (App (Id "if", cond), Lit 10), Lit 5), Lit 10;
  ] |> List.iter @@ fun (case, expected) ->
    let print_failed () = print_endline @@ "failed: " ^ string_of_t case in
    try
      let actual = eval pervasives case in
      if actual <> expected then (
        print_failed ();
        print_string "expected: ";
        print_endline @@ string_of_t expected;
        print_string "actual: ";
        print_endline @@ string_of_t actual;
        raise Exit
      )
    with Failure e ->
      print_failed ();
      print_endline e;
      raise Exit


let () =
  test_eval ();
  let program =
    App (App (Id "add", Lit 2), Lit 3)
  in
  print_endline @@ string_of_t program;
  print_endline @@ string_of_t @@ eval pervasives program
