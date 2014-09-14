# Derive a type representation for the dynamic inspection of values

```ocaml
open Repr

type 'a t = A of 'a | B [@@deriving Repr]

(** A generic [show] function based on the runtime representation of a value *)
let rec show = function
  | Dynamic (Int, v) ->
    string_of_int v
  | Dynamic (List t, v) ->
    "["^String.concat "; " (List.map (fun v -> show (dynamic t v)) v)^"]"
  | Dynamic (Variant { case_value }, v) ->
    match case_value v with 
      | Case_value (Case0 (n, _), ()) -> n
      | Case_value (Case (n, r, _), v) ->
        n^" "^show (dynamic r v)
    
print_endline @@ show @@
  dynamic [%derive.Repr: int t list] [A 3; B]
```
