let flip f x y = f y x

let is_some = function | Some _ -> true | None -> false

module Disjoint_set = Disjoint_set
