type t =
  Local of string * string
| Global of string

let to_string = function
  | Local (l,i) -> l ^ "." ^ i
  | Global i -> i

let local_of = function
  | Local (l,_) -> l
  | Global _ -> ""
