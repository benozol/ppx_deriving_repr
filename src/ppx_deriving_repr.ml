open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let affix = `Suffix "repr"

let rec core_repr typ =
  match typ with
  | [%type: unit] -> [%expr Repr.__unit__ ()]
  | [%type: int] -> [%expr Repr.__int__ ()]
  | [%type: int32] -> [%expr Repr.__int32__ ()]
  | [%type: int64] -> [%expr Repr.__int64__ ()]
  | [%type: nativeint] -> [%expr Repr.__nativeint__ ()]
  | [%type: float] -> [%expr Repr.__float__ ()]
  | [%type: bool] -> [%expr Repr.__bool__ ()]
  | [%type: char] -> [%expr Repr.__char__ ()]
  | [%type: string] -> [%expr Repr.__string__ ()]
  | [%type: bytes] -> [%expr Repr.__bytes__ ()]
  | [%type: exn] -> [%expr Repr.__exn__ ()]
  | [%type: [%t? typ] ref]   ->
     [%expr Repr.__ref__ [%e core_repr typ]]
  | [%type: [%t? typ] list] ->
     [%expr Repr.__list__ [%e core_repr typ]]
  | [%type: [%t? typ] array]   ->
     [%expr Repr.__array__ [%e core_repr typ]]
  | [%type: [%t? typ] option]   ->
     [%expr Repr.__option__ [%e core_repr typ]]
  | [%type: [%t? typ] List.t] ->
     [%expr Repr.__list__ [%e core_repr typ]]
  | [%type: [%t? typ] Array.t]   ->
     [%expr Repr.__array__ [%e core_repr typ]]
  | { ptyp_desc = Ptyp_any } ->
     [%expr Repr.__abstract__ () ]
  | { ptyp_desc = Ptyp_tuple typs } -> for_tuple typs
  | { ptyp_desc = Ptyp_arrow (label, ty1, ty2) } -> for_arrow label ty1 ty2
  | { ptyp_desc = Ptyp_constr (var, typs) } -> for_constr var typs
  | { ptyp_desc = Ptyp_var var } -> Exp.ident @@ lid ("poly_"^var)
  | { ptyp_desc = (Ptyp_object _ | Ptyp_class _ |
                      Ptyp_alias _ | Ptyp_variant _ | Ptyp_poly _ |
                      Ptyp_package _ | Ptyp_extension _) } ->
     Ppx_deriving.raise_errorf
       ("Cannot derive Repr: this type is not (yet) supported")

and for_constr {loc; txt} = function
  | [] ->
     Exp.ident @@ { loc ; txt = Ppx_deriving.mangle_lid affix txt}
  | typs ->
     let f =
       let txt = Ppx_deriving.mangle_lid affix txt in
       Exp.ident @@ { loc ; txt }
     in
     let arg =
       match typs with
         | [ typ ] -> core_repr typ
         | typs ->
            assert (typs <> []);
            Exp.tuple @@ List.map core_repr typs
     in
     Exp.apply f ["", arg]

and for_arrow label ty1 ty2 =
  let label =
    if label = ""
    then [%expr None]
    else
      if label.[0] <> '?' then
        [%expr Some { Repr.label; optional = false }]
      else
        let label = str @@ String.sub label 1 (String.length label) in
        [%expr Some { Repr.label = [%e label]; optional = true }]
  in
  [%expr Repr.__arrow__ ([%e label], [%e core_repr ty1], [%e core_repr ty2]) ]

and for_tuple typs =
  let component n typ =
    let project =
      let tuple_pattern =
        let component ix' _ =
          if ix' == n
          then [%pat? x]
          else Pat.any ()
        in
        Pat.tuple (List.mapi component typs)
      in
      Exp.fun_ "" None tuple_pattern [%expr x]
    in
    [%expr Repr.__component__ [%e core_repr typ] [%e project] ]
  in
  let components_list = list @@ List.mapi component typs in
  [%expr Repr.__tuple__ [%e components_list] ]

let for_record labels =
  let for_label { pld_name = {txt=name}; pld_mutable; pld_type } =
    let repr = core_repr pld_type in
    let getter = Exp.fun_ "" None (precord [name, pvar name]) (evar name) in
    let field = 
      match pld_mutable with
        | Immutable ->
           [%expr Repr.__field__ [%e repr] [%e getter] None ]
        | Mutable ->
           let setter =
             Exp.fun_  "" None (pvar "r") @@
               Exp.fun_ "" None (pvar "v") @@
                 Exp.setfield (evar "r") (lid name) (evar "v")
           in
           [%expr Repr.__field__ [%e repr] [%e getter] (Some [%e setter]) ]
    in
    Exp.tuple [str name; field]
  in
  let labels_list = list @@ List.map for_label labels in
  [%expr Repr.__record__ [%e labels_list ] ]

let for_variant constrs =
  let constr_vb { pcd_name={txt=name}; pcd_args=typs } =
    let case =
    match typs with
      | [] ->
         let create = Exp.fun_ "" None (punit ()) (constr name []) in
         [%expr Repr.__case0__ [%e str name] [%e create]]
      | [typ] ->
         let create = Exp.fun_ "" None (pvar "arg") (constr name [evar "arg"]) in
         [%expr Repr.__case__ [%e str name] [%e core_repr typ] [%e create]]
      | typs ->
         let create =
           let args = List.mapi (fun ix _ -> "arg"^string_of_int ix) typs in
           Exp.fun_ "" None
                    (ptuple @@ List.map pvar args)
                    (constr name @@ List.map evar args)
         in
         [%expr Repr.__case__ [%e str name] [%e for_tuple typs] [%e create]]
    in
    Vb.mk (pvar (String.uncapitalize name)) case
  in
  let variant =
    let cases = 
      let for_constr { pcd_name = {txt=name}} = 
        let case_var = evar @@ String.uncapitalize name in
        [%expr Repr.__any_case__ [%e case_var] ]
      in
      list @@ List.map for_constr constrs
    in
    let case =
      let get_case { pcd_name = {txt=name}; pcd_args = typs } =
        let case_var = evar @@ String.uncapitalize name in
        match typs with
        | [] ->
           pconstr name [],
           [%expr Repr.__any_case_value__ [%e case_var] () ]
        | [typ] ->
           pconstr name [pvar "arg"],
           [%expr Repr.__any_case_value__ [%e case_var] [%e evar "arg"] ]
        | typs ->
           let args = List.mapi (fun ix _ -> "arg"^string_of_int ix) typs in
           let evar_args = tuple @@ List.map evar args in
           pconstr name @@ List.map pvar args,
           [%expr Repr.__any_case_value__ [%e case_var] [%e evar_args] ]
      in
      func @@ List.map get_case constrs
    in
    [%expr Repr.__variant__ [%e cases] [%e case] ]
  in
  Exp.let_ Nonrecursive (List.map constr_vb constrs) variant

let str_of_type ~options ~path type_decl =
  let repr = 
    match type_decl.ptype_kind, type_decl.ptype_manifest with
      | Ptype_abstract, Some manifest -> core_repr manifest
      | Ptype_record labels, _ -> for_record labels
      | Ptype_variant constrs, _ -> for_variant constrs
      | _ -> assert false
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  [Vb.mk (pvar (Ppx_deriving.mangle_type_decl affix type_decl))
         (polymorphize repr)]

let sig_of_type ~options ~path ({ ptype_name = { txt = name } } as type_decl) =
  let polymorphize = Ppx_deriving.poly_arrow_of_type_decl (fun var -> [%type: [%t var] -> string]) type_decl in
  [Sig.value
     (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl affix type_decl))
             (polymorphize [%type: [%t tconstr name []] Repr.t ]))]

let () =
  let core_type typ =
    let fold expr name = Exp.fun_ "" None (pvar ("poly_"^name)) expr in
    let vars = Ppx_deriving.free_vars_in_core_type typ in
    let expr = [%expr ([%e core_repr typ] : [%t typ] Repr.t) ] in
    List.fold_left fold expr vars
  in
  let signature ~options ~path type_decls =
    List.concat (List.map (sig_of_type ~options ~path) type_decls)
  in
  let structure ~options ~path type_decls =
    [Str.value Recursive (List.concat (List.map (str_of_type ~options ~path) type_decls))]
  in
  Ppx_deriving.(register "Repr" { core_type; structure; signature })



