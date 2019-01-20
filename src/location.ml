open Lexing

type t = {
  start_pos : position;
  end_pos : position;
}

let pos_column pos = pos.pos_cnum - pos.pos_bol + 1

let print_position _outc pos =
  Printf.printf "%s:%i:%i" pos.pos_fname pos.pos_lnum (pos_column pos)

(* let print_loc outc { start_pos; end_pos } =
   Printf.printf "[%a - %a]" print_position start_pos print_position end_pos *)

let slice_opt ic =
  try Some (input_line ic)
  with End_of_file -> None

let nth_line n filename =
  let ic = open_in filename in
  let rec aux i =
    match slice_opt ic with
    | Some line ->
      if i = n then begin
        close_in ic;
        (line)
      end else aux (succ i)
    | None ->
      close_in ic;
      failwith "end of file reached"
  in
  aux 1

let slice_at_location loc =
  nth_line loc.start_pos.pos_lnum loc.start_pos.pos_fname

let print_loc loc =
  let open Printf in
  printf "%5s...\n" " ";
  printf "%6d| %s\n" loc.start_pos.pos_lnum (slice_at_location loc);
  printf "%6s|" " ";
  let offset = pos_column loc.start_pos in
  let width = (pos_column loc.end_pos - pos_column loc.start_pos) in
  printf "%*s%s" offset " " (String.make width '^')
