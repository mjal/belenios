open Cmdliner
open Belenios_core

let read_file filename =
  let channel = open_in filename in
  try
    let length = in_channel_length channel in
    let content = really_input_string channel length in
    close_in channel;
    content
  with e ->
    close_in_noerr channel;
    raise e

let ballot_str = read_file "ballot.json";;

let module G = (val Belenios_v1.Group.of_string "Ed25519") in

let n_to_string n = (Z.format "%x" (Z.of_string (G.Zq.to_string n)))
in

let checkSig (ballot : (G.t, G.Zq.t) Belenios_v1.Serializable_t.ballot) = 

  match ballot.signature with
  | None -> ()
  | Some { s_hash; s_proof = { challenge; response } } ->
    let commitment = G.(
      (g **~ response) *~ (ballot.credential **~ challenge)
    ) in
    let _ = s_hash in
    let () = Printf.printf "\n" in
    let () = Printf.printf "Challenge : %s\n" (n_to_string challenge) in
    let () = Printf.printf "Commitment (ou A?) : %s\n" (G.to_string commitment) in
    let make_sig_prefix hash = "sig|" ^ hash ^ "|" in
    let prefix = make_sig_prefix s_hash in
    let () = Printf.printf "hashedString : %s\n" (prefix ^ G.to_string commitment) in
    let _aa = Ed25519_copy.hash prefix [| (Ed25519_copy.of_string (G.to_string commitment)) |] in
    let recomputedChallenge = G.hash prefix [| commitment |] in
    let () = Printf.printf "recomputedChallenge : %s\n" (n_to_string recomputedChallenge) in
    ()
in

let main _ballot =
  let ballot = Belenios_v1.Serializable_j.ballot_of_string
    (Common.sread G.of_string) (Common.sread G.Zq.of_string) ballot_str in

  let () = checkSig ballot in

  let g = G.g in
  let () = Printf.printf "g : %s\n" (G.to_string (g)) in
  let () = Printf.printf "h : %s\n" (G.to_string (G.invert g)) in
  ()
in

(* Program setup *)
let setup =
  let doc = "A serialized ballot." in
  let ballot =
    let docv = "BALLOT" in
    Arg.(
      value & opt (some string) None & info [ "c"; "count" ] ~docv ~doc)
  in
  Term.(const main $ ballot)
in

let info = Cmd.info "belenios-minilib" ~version:"0.1" ~doc:"" ~man: []
in

let () = Cmd.eval (Cmd.v info setup) |> exit in
()
