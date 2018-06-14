type 'a t = {
  parent : 'a t ref;
  rank : int ref;
  size : int ref;
  elem : 'a;
  root : bool;
}

exception Incompatible_type of string

let make elem ~root =
  let rec x = {
    parent = ref x;
    rank = ref 0;
    size = ref 1;
    root;
    elem
  } in x

let rec find x =
  let deref_parent = !(x.parent) in
  if deref_parent != x then x.parent := find(deref_parent);
  !(x.parent)

let union x y =
  let x_root = find(x) in
  let y_root = find(y) in

  if x_root.root && y_root.root then
    let err_msg = Printf.sprintf "Incompatible types! Type %s is not compatible with type %s" 
        (Printer.type_to_str x_root.elem)
        (Printer.type_to_str y_root.elem) in
    raise (Incompatible_type err_msg)
  else begin
    if x_root.elem <> y_root.elem then
      let (x_root, y_root) = if not x_root.root && (x_root).rank < (y_root).rank
        then (y_root, x_root) else (x_root, y_root) in 
      y_root.parent := x_root;
      x_root.size := !(x_root.size) + !(y_root.size);
      if x_root.rank = y_root.rank then incr x_root.rank
  end
