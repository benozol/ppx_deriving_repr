open Ocamlbuild_plugin

let () =
  dispatch begin
      function
        | After_rules ->
           flag ["ocaml"; "compile"; "use_repr"] &
             S[A"-ppx"; A"ocamlfind ppx_deriving/ppx_deriving src/ppx_deriving_repr.cma"];
        | _ -> ()
    end
