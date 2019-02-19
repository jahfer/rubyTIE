open Cmdliner

let rbt filename =
  `Ok(Ruby_tie.parse_from_filename filename)

let srcs =
  let doc = "Source file(s) to copy." in
  Arg.(required & ((pos 0 (some string) None)) & (info [] ~doc))

let cmd =
  let doc = "Parse and display Ruby" in
  let exits = Term.default_exits in
  Term.(ret (const rbt $ srcs)),
  Term.info "rbt" ~version:"v1.0.2" ~exits ~doc

let () =
  Term.(exit @@ eval cmd)

