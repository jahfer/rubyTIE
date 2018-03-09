open Core
open Lexing

type t = {
  start_pos : position;
  end_pos : position;
}

let print_position outc pos =
  let offset = pos.pos_cnum - pos.pos_bol in
  printf "%s:%i:%i" pos.pos_fname pos.pos_lnum (offset + 1)

let print_loc outc { start_pos; end_pos } =
  printf "[%a - %a]" print_position start_pos print_position end_pos
