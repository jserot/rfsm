open Printf

let level = ref 0

let print_tab l = printf ">%s" (String.make (l*2) ' ')
                
let msg0 l m = if !level >= l then begin print_tab l; printf m; flush stdout end
let msg1 l m x = if !level >= l then begin print_tab l; printf m x; flush stdout end
let msg2 l m x y = if !level >= l then begin print_tab l; printf m x y; flush stdout end
let msg3 l m x y z = if !level >= l then begin print_tab l; printf m x y z; flush stdout end
