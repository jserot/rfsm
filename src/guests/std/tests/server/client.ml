let start_client sockpath client_fun  =
    let sockadr = Unix.ADDR_UNIX sockpath in
    let ic,oc = Unix.open_connection sockadr in
    Printf.printf "Client connection opened on %s\n" sockpath; flush stdout;
    client_fun sockpath ic oc

let close_connection sockpath (ic,oc) = 
       Printf.printf "Client: closing connection\n"; flush stdout;
       Unix.shutdown_connection ic;
       close_out oc; (* Required to close the socket *)
       Unix.unlink sockpath (* Deleting the socket file *)

let client_fun sockpath ic oc =
  try
    while true do
      print_string  "Request : " ;
      flush stdout ;
      output_string oc ((input_line stdin)^"\n") ;
      flush oc ;
      let r = input_line ic in
      Printf.printf "Answer : %s\n" r;
      if r = "QUIT" then raise Exit
     done
   with
   | Exit ->
       Printf.printf "Client got exit answer\n"; flush stdout;
       close_connection sockpath (ic,oc);
       exit 0
   | exn ->
       Printf.printf "Client got exn %s\n" (Printexc.to_string exn); flush stdout;
       close_connection sockpath (ic,oc);
       exit 1

let main () = 
  if Array.length Sys.argv < 2
  then Printf.printf "usage :  %s socket_path\n" Sys.argv.(0);
  start_client Sys.argv.(1) client_fun

let _ = main ()

