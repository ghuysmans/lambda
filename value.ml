type id = string

(* TODO make a GADT of it just for fun! *)
type t =
  | Id of id (* identifier *)
  | Lit of int (* literal *)
  | Abs of id * t (* abstraction (function) *)
  | App of t * t (* application *)
  | Native of (env -> t) (* native function *)
and env =
  (id * t) list

let get env id =
  if List.mem_assoc id env then
    List.assoc id env
  else
    failwith @@ "Unbound variable " ^ id

(* FIXME does this halt? *)
let rec string_of_env env =
  env |>
  List.map (fun (id, e) -> id ^ " = " ^ string_of_t e) |>
  String.concat ", "
and string_of_t = function
  | Id id -> id
  | Lit lit -> string_of_int lit
  | Native _ -> "<native>"
  | Abs (id, e) -> "(\\" ^ id ^ ". " ^ string_of_t e ^ ")"
  | App (f, e) -> "(" ^ string_of_t f ^ " " ^ string_of_t e ^ ")"

let type_error x expected =
  failwith @@ "Type error: `" ^ string_of_t x ^ "` is not " ^ expected ^ "."

let int_of_t env id =
  match get env id with
  | Lit lit -> lit
  | x -> type_error x "an int"

let curry f =
  List.fold_left (fun acc arg -> App (acc, arg)) f

let rec eval env = function
  | Id id ->
    (* TODO recurse? *)
    get env id
  | Lit _ as lit ->
    (* fully evaluated *)
    lit
  | Native f as n ->
    (* hackish *)
    f env
  | Abs _ as a ->
    (* we can't do anything yet *)
    a
  | App (f, e) ->
    match f with
    | Id id ->
      (* FIXME I don't think it always terminates... *)
      eval env (App (get env id, e))
    | Abs (id, b) ->
      let param = eval env e in
      let env' = (id, param) :: env in
      eval env' b
    | x ->
      type_error x "a function"
