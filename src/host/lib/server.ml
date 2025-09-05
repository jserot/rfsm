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

let establish_server server_fun sockaddr =
  (* This is a variant of [Unix.establish_server] accepting a single connection *)
  let domain = Unix.domain_of_sockaddr sockaddr in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 
  in Unix.bind sock sockaddr ;
     Unix.listen sock 3;
     let (s, _) = Unix.accept sock 
     in match Unix.fork() with
          0 -> if Unix.fork() <> 0 then exit 0 ; 
               let inchan = Unix.in_channel_of_descr s 
               and outchan = Unix.out_channel_of_descr s 
               in server_fun inchan outchan ;
                  close_in inchan ;
                  close_out outchan ;
                  exit 0
          | id -> Unix.close s; ignore(Unix.waitpid [] id)

let start ~socket ~fn =
  Printf.printf "server: starting on %s\n" socket; flush stdout;
  establish_server fn (Unix.ADDR_UNIX socket)

