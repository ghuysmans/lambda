type id = string

(* TODO make a GADT of it just for fun! *)
type t =
  | Id of id (* identifier *)
  | Lit of int (* literal *)
  | Native of ((string * t) list -> t) (* trick, see eval *)
  | Abs of id * t (* abstraction (function) *)
  | App of t * t (* application *)


let curry f =
  List.fold_left (fun acc arg -> App (acc, arg)) f

let get env id =
  if List.mem_assoc id env then
    List.assoc id env
  else
    failwith @@ "Unbound variable " ^ id

let rec to_string = function
  | Id id -> id
  | Lit lit -> string_of_int lit
  | Native _ -> "<native>"
  | Abs (id, e) -> "(\\" ^ id ^ ". " ^ to_string e ^ ")"
  | App (f, e) -> "(" ^ to_string f ^ " " ^ to_string e ^ ")"

let get_int env id =
  match get env id with
  | Lit lit -> lit
  | x -> failwith @@ "Type error: `" ^ to_string x ^ "` is not an int."

let pervasives = [
  "add", Abs ("x", Abs("y", Native (fun env -> Lit (get_int env "x" + get_int env "y"))));
  "gt", Abs ("x", Abs("y", Native (fun env -> Lit (if get_int env "x" > get_int env "y" then 1 else 0))));
  "if", Abs ("c", Abs("t", Abs("f", Native (fun env -> get env (if get_int env "c" = 0 then "f" else "t")))));
  "print", Abs ("x", Native (fun env -> print_endline @@ to_string @@ get env "x"; Lit 0))
]

let rec dump l =
  if l == pervasives || l = [] then
    (* base case: default environment or the empty one *)
    (* Default values are hidden since we always prepend! *)
    ()
  else
    let id, e = List.hd l in
    print_endline @@ "* " ^ id ^ " = " ^ to_string e;
    (* continue *)
    dump (List.tl l)

let rec eval ?(real=true) env = function
  | Id id ->
    (* TODO recurse? *)
    get env id
  | Lit _ as lit ->
    (* fully evaluated *)
    lit
  | Native f as n ->
    if real then
      f env
    else
      n
  | Abs _ as a ->
    (* we can't do anything yet *)
    a
  | App (f, e) ->
    match eval ~real:false env f with
    | Abs (id, b) ->
      let param = eval env e in
      let env' = (id, param) :: env in
      eval env' b
    | x ->
      failwith @@ "Type error: `" ^ to_string x ^ "` is not a function."

let test_eval () =
  [
    Lit 2, Lit 2;
    (*App (Abs ("x", Id "x"), Lit 2), Lit 2;*)
    App (App (Id "add", Lit 2), Lit 2), Lit 4;
    App (App (Id "gt", Lit 1), Lit 2), Lit 0;
    App (App (App (Id "if", Lit 0), Lit 2), Lit 3), Lit 0;
    let cond = App (App (Id "gt", Lit 3), Lit 1) in
      App (App (App (Id "if", cond), Lit 10), Lit 5), Lit 10;
  ] |> List.iter @@ fun (case, expected) ->
    let print_failed () = print_endline @@ "failed: " ^ to_string case in
    try
      let actual = eval pervasives case in
      if actual <> expected then (
        print_failed ();
        print_string "expected: ";
        print_endline @@ to_string expected;
        print_string "actual: ";
        print_endline @@ to_string actual;
        raise Exit
      )
    with Failure e ->
      print_failed ();
      print_endline e;
      raise Exit


let () =
  test_eval ()
  (*
  let program =
    let id = Abs ("x", Id "x") in
    App (id, App (id, Lit 42))
  in
  print_endline @@ to_string program;
  print_endline @@ to_string @@ eval pervasives program
  *)
