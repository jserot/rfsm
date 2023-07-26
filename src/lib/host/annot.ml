type ('a,'b) t = {
      mutable desc: 'a; [@main]
      mutable typ: 'b;  (* Just a placeholder here, since the associated type will be defined in the Guest language *)
      loc: Location.t [@default Location.no_location]
      } [@@deriving map]
