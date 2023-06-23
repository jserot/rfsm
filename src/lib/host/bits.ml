let of_uint s n = 
  let b = Bytes.make s '0' in
  let rec h n i =
    if i >= 0 then begin
      Bytes.set b i (if n mod 2 = 1 then '1' else '0');
      h (n/2) (i-1)
      end in
  h n (s-1);
  Bytes.to_string b

let cpl2 n x =
  let rec pow2 k = if k = 0 then 1 else 2 * pow2 (k-1) in (* Not tail recursive, but who cares, here ... *)
  pow2 n - x

let of_int s v = 
  if v < 0 
  then of_uint s (cpl2 s (-v))
  else of_uint s v

let get_bits ~hi ~lo n = (n lsr lo) mod (1 lsl (hi-lo+1))

let set_bits ~hi ~lo ~dst v =
  let v' = v mod (1 lsl (hi-lo+1)) in
  let msk = let r = ref 0 in for i=lo to hi do r := !r lor (1 lsl i) done; !r in
  (dst land (lnot msk)) lor (v' lsl lo)
