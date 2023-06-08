type ('a,'b) t = {
      desc: 'a; [@main]
      mutable typ: 'b option; [@default None]
      loc: Location.t [@default Location.no_location]
      } [@@deriving make, map]
