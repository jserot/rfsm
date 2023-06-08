(** Dated event sets *)

module type T = sig
  
  module Event: Event.T
  (* module Evset: Set.S with type elt = Event.t *)

  type date = int 
  (* type t = date * Evset.t *)
  type t

  exception Union of t * t
                   
  val mk: date -> Event.t list -> t

  val empty: date -> t
    
  val date: t -> date
  val events: t -> Event.t list

  val is_empty: t -> bool
    
  exception Add of Event.t

  val add: t -> Event.t -> t
    
  val union: t -> t -> t
  val union_all: date -> t list -> t
    
  val partition: f:(Event.t->bool) -> t -> t * t

  val pp: Format.formatter -> t -> unit

end

module Make (E: Event.T)
       : T with module Event = E =
struct

  module Event = E
  module Evset = Set.Make(Event)

  type date = int
  type t = date * Evset.t

  let mk t evs = t, Evset.of_list evs

  let empty t = t, Evset.empty

  let is_empty (_,evs) = Evset.is_empty evs

  let date (t,_) = t
  let events (_,evs) = Evset.elements evs 

  exception Add of Event.t

  let add (t,evs) s =
    if Evset.mem s evs then raise (Add s)
    else t, Evset.add s evs

  exception Union of t * t

  let union ((t1,evs1) as e1) ((t2,evs2) as e2) =
    if t1 = t2 && Evset.disjoint evs1 evs2 then
      t1, Evset.union evs1 evs2
    else
      raise (Union (e1,e2))

  let union_all t evs = match evs with
    | [] -> empty t
    | ev::evs -> List.fold_left union ev evs
    
  let partition ~f (t,evs)  =
    let evs1, evs2 = Evset.partition f evs in
    (t, evs1),
    (t, evs2)

  let pp fmt (t,evs) =
    let open Format in
    let pp_evs fmt evs = pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",") E.pp fmt evs in
    fprintf fmt "{%a}@%d" pp_evs (Evset.elements evs) t

end
