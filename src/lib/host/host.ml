(* The host language, for describing "abstract" RFSMs *)

module type T = sig
  module Guest: Guest.T
  module Syntax: Syntax.SYNTAX 
  module Typing: Typing.TYPING
  module Static: Static.T 
  module Dot: Dot.DOT with module Static = Static
  module Dynamic: Dynamic.DYNAMIC with module Syntax = Syntax and module Static = Static
  module Vcd: Vcd.VCD 
  module Ctask: Ctask.CTASK with module Static = Static
  module Systemc: Systemc.SYSTEMC with module Static = Static
  module Vhdl: Vhdl.VHDL with module Static = Static
  val type_program: Typing.env -> Syntax.program -> Typing.typed_program
  val elab: Typing.typed_program -> Static.t
  val run: ?vcd_file:string -> Syntax.program -> Static.t -> unit
  val pp_program: Format.formatter -> Syntax.program -> unit
  val pp_tenv: Format.formatter -> Typing.env -> unit
end

module Make (G: Guest.T)
       : T with module Guest = G
            and module Syntax = Syntax.Make(G.Syntax) =
struct
    module Guest = G
    module Syntax = Syntax.Make(G.Syntax)
    module Typing = Typing.Make(Syntax)(G.Typing)(G.Static)
    module Static = Static.Make(Syntax)(G.Value)(G.Static)
    module Dot = Dot.Make(Static)
    module Dynamic = Dynamic.Make(Syntax)(Static)(G.Eval)
    module Vcd = Vcd.Make(Dynamic.EvSeq)
    module Cmodel = Cmodel.Make(Static)
    module Ctask = Ctask.Make(Static)(G.Ctask)
    module Systemc = Systemc.Make(Static)(G.Systemc)
    module Vhdl = Vhdl.Make(Static)(G.Vhdl)

    let type_program tenv p = Typing.type_program tenv p

    let elab p = Static.build p

    let run ?(vcd_file="") p s =
      let rs = Dynamic.run p s in
      if vcd_file <> "" then begin
          Vcd.output ~fname:vcd_file rs;
          Printf.fprintf stdout "Wrote %s\n" vcd_file
        end

    let pp_program p = Syntax.pp_program p
    let pp_tenv fmt te = Typing.pp_env fmt te
  end
