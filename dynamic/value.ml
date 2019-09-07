type id = string

(* TODO make a GADT of it just for fun! *)
type t =
  | Id of id (* identifier *)
  | Lit of int (* literal *)
  | Abs of id * t (* abstraction (function) *)
  | App of t * t (* application *)
  | Closure of env * t (* partial application *)
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
  String.concat ", " |> fun l ->
  "[" ^ l ^ "]"
and string_of_t = function
  | Id id -> id
  | Lit lit -> string_of_int lit
  | Abs (id, e) -> "(\\" ^ id ^ ". " ^ string_of_t e ^ ")"
  | App (f, e) -> "(" ^ string_of_t f ^ " " ^ string_of_t e ^ ")"
  | Closure (e, x) -> "(" ^ string_of_t x ^ ") " ^ string_of_env e
  | Native _ -> "<native>"

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
  | Abs _ as a ->
    (* we can't do anything yet *)
    a
  | Native f ->
    (* hackish *)
    f env
  | Closure (_, Abs _) as c ->
    (* not completely applied yet, so let's leave it alone *)
    c
  | Closure (env, e) ->
    (* "reduce" other closures *)
    eval env e
  | App (f, param) ->
    let f = eval env f in (* avoid identifiers... *)
    let param = eval env param in (* evaluate the argument first *)
    match f with
    | Abs (id, e) ->
      (* create a closure *)
      let env' = (id, param) :: env in
      eval env' (Closure (env', e))
    | Closure (env, Abs (id, e)) ->
      (* extend an existing closure *)
      let env' = (id, param) :: env in
      eval env' (Closure (env', e))
    | x ->
      (* must be an error *)
      type_error x "a function"
