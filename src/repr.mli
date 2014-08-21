
type _ t = private
  | Abstract : _ t
  | Unit : unit t
  | Bool : bool t
  | Float : float t
  | Char : char t
  | String : string t
  | Int : int t
  | Int32 : int32 t
  | Int64 : int64 t
  | Nativeint : nativeint t
  | Bytes : bytes t
  | Exn : exn t
  | List : 'a t -> 'a list t
  | Array : 'a t -> 'a array t
  | Ref : 'a t -> 'a ref t
  | Option : 'a t -> 'a option t
  | Tuple : 'a tuple -> 'a t
  | Arrow : label option * 'a t * 'b t -> ('a -> 'b) t 
  | Record : 'a record -> 'a t
  | Variant : 'a variant -> 'a t
and label = private {
  label : string;
  optional : bool;
}

and 'a tuple = private {
  components : 'a any_component list;
}
and ('a, 'b) component = private {
  repr : 'b t;
  project : 'a -> 'b;
}
and 'a any_component = private
  | Any_component : ('a, _) component -> 'a any_component

and 'a record = private {
  fields : (string * 'a any_field) list;
}
and ('a, 'b) field = private {
  repr : 'b t;
  getter : 'a -> 'b;
  setter : ('a -> 'b -> unit) option;
}
and 'a any_field = private
   | Any_field : ('a, _) field -> 'a any_field

and 'a variant = {
  cases : 'a any_case list;
  case : 'a -> 'a any_case_value;
}
and ('a, 'b) case =
  | Case0 : string * (unit -> 'a) -> ('a, unit) case
  | Case : string * 'b t * ('b -> 'a) -> ('a, 'b) case
and 'a any_case = Any_case : ('a, _) case -> 'a any_case
and 'a any_case_value = Any_case_value : ('a, 'b) case * 'b -> 'a any_case_value

type dynamic = private 
   | Dynamic : 'a t * 'a -> dynamic
val dynamic : 'a t -> 'a -> dynamic

(**/**)

val __abstract__ : unit -> _ t
val __unit__ : unit -> unit t
val __int__ : unit -> int t
val __int32__ : unit -> int32 t
val __int64__ : unit -> int64 t
val __nativeint__ : unit -> nativeint t
val __float__ : unit -> float t
val __bool__ : unit -> bool t
val __char__ : unit -> char t
val __string__ : unit -> string t
val __bytes__ : unit -> bytes t
val __exn__ : unit -> exn t
val __ref__ : 'a t -> 'a ref t
val __list__ : 'a t -> 'a list t
val __array__ : 'a t -> 'a array t
val __option__ : 'a t -> 'a option t
val __arrow__ : label option -> 'a t -> 'b t  -> ('a -> 'b) t

val __tuple__ : 'a any_component list -> 'a t
val __component__ : 'b t -> ('a -> 'b) -> 'a any_component

val __record__ : (string * 'a any_field) list -> 'a t
val __field__ : 'b t -> ('a -> 'b) -> ('a -> 'b -> unit) option -> 'a any_field

val __variant__ : 'a any_case list -> ('a -> 'a any_case_value) -> 'a t
val __any_case__ : ('a, _) case -> 'a any_case
val __case0__ : string -> (unit -> 'a) -> ('a, unit) case
val __case__ : string -> 'b t -> ('b -> 'a) -> ('a, 'b) case
val __any_case_value__ : ('a, 'b) case -> 'b -> 'a any_case_value
