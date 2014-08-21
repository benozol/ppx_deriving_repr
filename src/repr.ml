type _ t =
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
and label = {
  label : string;
  optional : bool;
}

and 'a tuple = {
  components : 'a any_component list;
}
and ('a, 'b) component = {
  repr : 'b t;
  project : 'a -> 'b;
}
and 'a any_component = Any_component : ('a, _) component -> 'a any_component

and 'a record = {
  fields : (string * 'a any_field) list;
}
and ('a, 'b) field = {
  repr : 'b t;
  getter : 'a -> 'b;
  setter : ('a -> 'b -> unit) option;
}
and 'a any_field = Any_field : ('a, _) field -> 'a any_field

and 'a variant = {
  cases : 'a any_case list;
  case : 'a -> 'a any_case_value
}
and ('a, 'b) case =
  | Case0 : string * (unit -> 'a) -> ('a, unit) case
  | Case : string * 'b t * ('b -> 'a) -> ('a, 'b) case
and 'a any_case = Any_case : ('a, _) case -> 'a any_case
and 'a any_case_value = Any_case_value : ('a, 'b) case * 'b -> 'a any_case_value

type dynamic = Dynamic : 'a t * 'a -> dynamic
let dynamic repr value = Dynamic (repr, value)

let __abstract__ () = Abstract
let __unit__ () = Unit
let __bool__ () = Bool
let __float__ () = Float
let __char__ () = Char
let __string__ () = String
let __int__ () = Int
let __int32__ () = Int32
let __int64__ () = Int64
let __nativeint__ () = Nativeint
let __exn__ () = Exn
let __list__ repr = List repr
let __array__ repr = Array repr
let __bytes__ () = Bytes
let __ref__ repr = Ref repr
let __option__ repr = Option repr
let __tuple__ components = Tuple { components }
let __component__ repr project = Any_component { repr; project }
let __arrow__ label repr1 repr2  = Arrow (label, repr1, repr2)
let __record__ fields = Record { fields }
let __field__ repr getter setter = Any_field { repr; getter; setter }
let __variant__ cases case = Variant { cases; case }
let __case0__ name create = Case0 (name, create)
let __case__ name repr create = Case (name, repr, create)
let __any_case__ case = Any_case case
let __any_case_value__ case value = Any_case_value (case, value)
