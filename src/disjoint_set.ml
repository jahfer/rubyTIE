(* let rec occurs u v = false *)

type 'a t = {
  parent : 'a t ref;
  rank : int ref;
  size : int ref;
  elem : 'a;
}

let make elem =
  let rec x = {
    parent = ref x;
    rank = ref 0;
    size = ref 1;
    elem 
  } in x

let rec find x =
  let deref_parent = !(x.parent) in
  if deref_parent != x then x.parent := find(deref_parent);
  !(x.parent)

let union x y =
  let x_root = find(x) in
  let y_root = find(y) in

  if x_root.elem <> y_root.elem then
    let (x_root, y_root) = if (x_root).rank < (y_root).rank 
                           then (y_root, x_root) else (x_root, y_root) in 
    y_root.parent := x_root;
    x_root.size := !(x_root.size) + !(y_root.size);
    if x_root.rank = y_root.rank then incr x_root.rank
