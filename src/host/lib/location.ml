(**********************************************************************)
(*                                                                    *)
(*              This file is part of the RFSM package                 *)
(*                                                                    *)
(*  Copyright (c) 2018-present, Jocelyn SEROT.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the license found in the       *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

(* Largely inspired from the file location.ml found in the Caml Light 0.75 distribution  *)

open Lexing
open Parsing

let input_name = ref ""                       (* Input file name. *)
and input_chan = ref stdin                    (* The channel opened on the input. *)
and input_lexbuf = ref (Obj.magic 0 : lexbuf) (* The lexer buffer on the input. *)

let error_prompt = ">"

type t =
    Loc of string  (* Filename *)
         * int     (* Position of the first character *)
         * int     (* Position of the next character following the last one *)
    (* [@@deriving show {with_path=false}] *)

let no_location = Loc("",0,0)

let get_current_location () =
  Loc (!input_name, symbol_start(), symbol_end())

let output_lines fmt char1 char2 charline1 line1 line2 =
  begin
	let n1 = char1 - charline1
	and n2 = char2 - charline1 in
	if line2 > line1 then
	  Format.fprintf fmt ", line %d-%d, characters %d-%d:\n" line1 line2 n1 n2
	else
	  Format.fprintf fmt ", line %d, characters %d-%d:\n" line1 n1 n2;
	()
  end

let output_loc fmt input seek line_flag (Loc(_,pos1, pos2)) =
  let pr_chars n c =
    for _ = 1 to n do Format.pp_print_char fmt c done in
  let skip_line () =
    try
      while input() != '\n' do () done
    with End_of_file -> () in
  let copy_line () =
    let c = ref ' ' in
      begin try
        while c := input(); !c != '\n' do Format.pp_print_char fmt !c done
      with End_of_file ->
        Format.pp_print_string fmt "<EOF>"
      end;
      Format.pp_print_char fmt '\n' in
  let pr_line first len ch =
    let c = ref ' '
    and f = ref first
    and l = ref len in
      try
        while c := input (); !c != '\n' do
	  if !f > 0 then begin
            f := !f - 1;
            Format.pp_print_char fmt (if !c == '\t' then !c else ' ')
          end
          else if !l > 0 then begin
            l := !l - 1;
            Format.pp_print_char fmt (if !c == '\t' then !c else ch)
          end
          else ()
        done
      with End_of_file ->
        if !f = 0 && !l > 0 then pr_chars 5 ch in
  let pos = ref 0
  and line1 = ref 1
  and line1_pos = ref 0
  and line2 = ref 1
  and line2_pos = ref 0 in
  seek 0;
  begin try
    while !pos < pos1 do
      incr pos;
      if input() == '\n' then begin incr line1; line1_pos := !pos; () end
    done
  with End_of_file -> ()
  end;
  line2 := !line1;
  line2_pos := !line1_pos;
  begin try
    while !pos < pos2 do
      incr pos;
      if input() == '\n' then
        begin incr line2; line2_pos := !pos; () end
    done
  with End_of_file -> ()
  end;
  if line_flag then output_lines fmt pos1 pos2 !line1_pos !line1 !line2;
  if !line1 == !line2 then begin
    seek !line1_pos;
    Format.pp_print_string fmt error_prompt;
    copy_line ();
    seek !line1_pos;
    Format.pp_print_string fmt error_prompt;
    pr_line (pos1 - !line1_pos) (pos2 - pos1) '^';
    Format.pp_print_char fmt '\n'
  end else begin
    seek !line1_pos;
    Format.pp_print_string fmt error_prompt;
    pr_line 0 (pos1 - !line1_pos) '.';
    seek pos1;
    copy_line();
    if !line2 - !line1 <= 8 then
      for _ = !line1 + 1 to !line2 - 1 do
        Format.pp_print_string fmt error_prompt;
        copy_line()
      done
    else
      begin
        for _ = !line1 + 1 to !line1 + 3 do
          Format.pp_print_string fmt error_prompt;
          copy_line()
        done;
        Format.pp_print_string fmt error_prompt; Format.pp_print_string fmt "..........\n";
        for _ = !line1 + 4 to !line2 - 4 do skip_line() done;
        for _ = !line2 - 3 to !line2 - 1 do
          Format.pp_print_string fmt error_prompt;
          copy_line()
        done
      end;
    begin try
      Format.pp_print_string fmt error_prompt;
      for _ = !line2_pos to pos2 - 1 do
        Format.pp_print_char fmt (input())
      done;
      pr_line 0 100 '.'
    with End_of_file -> Format.pp_print_string fmt "<EOF>"
    end;
    Format.pp_print_char fmt '\n'
  end

let pp_location fmt ((Loc (filename,_,_)) as loc) =
  if String.length !input_name > 0 then begin
    let fname, chan = 
      if filename <> !input_name then (* This may happen when [pp_location] is called after the parsing step.. *)
        filename, open_in filename                                                                                      
      else
        filename,
        !input_chan in
    let p = pos_in !input_chan in  (* When operating on the current input channel, we must save the current position *)
    Format.fprintf fmt "File \"%s\"" fname;
    output_loc
      fmt (fun () -> input_char chan) (seek_in chan) true
      loc;
    seek_in !input_chan p
  end else begin
    Format.fprintf fmt "Toplevel input:\n";
    let curr_pos = ref 0 in
    let input () =
      let c =
        if !curr_pos >= 2048 then
          raise End_of_file
        else if !curr_pos >= 0 then
          Bytes.get !input_lexbuf.lex_buffer !curr_pos
        else
          '.'
      in
        incr curr_pos; c
    and seek pos =
      curr_pos := pos - !input_lexbuf.lex_abs_pos
    in
      output_loc fmt input seek false loc
  end

let pp_input_name fmt =
  Format.fprintf fmt "File \"%s\", line 1:\n" !input_name

let string_of_location (Loc (f,c1,c2)) = Format.sprintf "%s:%d-%d" f c1 c2

let text_of_location (Loc (f,c1,c2)) = 
  let ic = open_in f in
  try 
    seek_in ic c1;
    let s = really_input_string ic (c2-c1+1) in
    close_in ic;
    s
  with
    Invalid_argument _ ->
    close_in ic;
    ""

  
