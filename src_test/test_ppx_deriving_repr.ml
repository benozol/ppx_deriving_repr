open OUnit2

(* type t1 = int [@@deriving Ty] *)
(* type 'a t2 = { f : int; g: 'a list } [@@deriving Ty] *)
(* type 'a t3 = 'a list [@@deriving Ty] *)
type ('a, 'b) t4 = A | B of int | C of 'a * 'b [@@deriving Repr]

(* let f (a, b) = C (a, b) *)

(* let t1 = [%derive.Ty: int] *)
(* let t2 = [%derive.Ty: unit list] *)
(* let t3 = [%derive.Ty: unit list] *)
(* let t4 = [%derive.Ty: (int * bool)] *)
(* let t4 = [%derive.Ty: int t3 * unit t3] *)

let suite = "test deriving(repr)" >::: [
    (* "test_simple" >:: test_simple; *)
  ]
