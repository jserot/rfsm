let rec bit_size n = if n=0 then 0 else 1 + bit_size (n/2)

let list_split_at n l =
  let rec h n left l = match n, l with
      0, rest -> left, rest
    | n, [] -> invalid_arg "list_split_at"
    | n, x::xs -> h (n-1) (left@[x]) xs in
  h n [] l
      
