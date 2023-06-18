(* The host language, for describing "abstract" RFSMs *)

module type T = sig
  module Guest: Guest.T
  module Syntax: Syntax.SYNTAX 
  module Typing: Typing.TYPING
  module Static: Static.STATIC 
  module Dot: Dot.DOT with module Static = Static
  module Dynamic: Dynamic.DYNAMIC with module Syntax = Syntax
  module Vcd: Vcd.VCD 
  val type_program: Typing.env -> Syntax.program -> unit
  val elab: Syntax.program -> Static.t
  (* val draw: dir:string -> name:string -> Static.t -> unit *)
  val run: ?verbose_level:int -> ?vcd_file:string -> Syntax.program -> Static.t -> unit
  val pp_program: Format.formatter -> Syntax.program -> unit
  val pp_tenv: Format.formatter -> Typing.env -> unit
end

module Make (G: Guest.T)
       : T with module Guest = G
            and module Syntax = Syntax.Make(G.Syntax)
= struct
    module Guest = G
    module Syntax = Syntax.Make(G.Syntax)
    module Typing = Typing.Make(Syntax)(G.Typing)
    module Static = Static.Make(Syntax)(G.Typing)(G.Value)(G.Static)
    module Dot = Dot.Make(Static)
    module Dynamic = Dynamic.Make(Syntax)(Static)(G.Eval)
    module Vcd = Vcd.Make(Dynamic.Seq)

    let type_program tenv p = Typing.type_program tenv p

    let elab p = Static.build p

    (* let draw ~dir ~name s =
     *   let fnames = Dot.output_static ~dir ~name s in
     *   List.iter *)

    let run ?(verbose_level=0) ?(vcd_file="") p s =
      let rs = Dynamic.run ~verbose_level p s in
      if vcd_file <> "" then begin
          Vcd.output ~fname:vcd_file rs;
          Printf.fprintf stdout "Wrote %s\n" vcd_file
        end

    let pp_program p = Syntax.pp_program p
    let pp_tenv fmt te = Typing.pp_env fmt te
  end