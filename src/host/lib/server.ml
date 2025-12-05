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

let establish_server port =
  let open Unix in
  let sockaddr = ADDR_INET (inet_addr_loopback, port) in
  (* Create TCP socket *)
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock sockaddr;
  listen sock 1;
  if !Options.verbose then Printf.printf "rfsm server: listening on 127.0.0.1:%d\n%!" port;
  (* Accept ONE client only *)
  let (client_sock, _) = accept sock in
  if !Options.verbose then Printf.printf "rfsm server: client connected\n%!";
  let in_ch  = in_channel_of_descr client_sock in
  let out_ch = out_channel_of_descr client_sock in
  (in_ch, out_ch)
  (* let rec loop () =
   *   match input_line in_ch with
   *   | line ->
   *       handler line out_ch;
   *       flush out_ch;
   *       loop ()     (\* continue *\)
   *   | exception End_of_file ->
   *       Printf.printf "server: client disconnected\n%!";
   *       ()
   * in
   * loop ();
   * (\* Cleanup *\)
   * close_in_noerr in_ch;
   * close_out_noerr out_ch;
   * (try close client_sock with _ -> ());
   * Printf.printf "server: shutting down\n%!" *)

let start ~socket_port ~service =
  let ic, oc = establish_server socket_port in
  service ic oc

