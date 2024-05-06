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

module PSerializable_j = Serializable_j
open Belenios_core
let main _ballot =
  let module G = (val Belenios_v1.Group.of_string "Ed25519") in
  let ballot = Belenios_v1.Serializable_j.ballot_of_string
    (Common.sread G.of_string) (Common.sread G.Zq.of_string) ballot_str in
    (*(string_of_ballot (swrite G.to_string) (swrite G.Zq.to_string)*)

  match ballot.signature with
  | Some { s_hash; s_proof = { challenge; response } } ->
    let challenge_int =  (G.Zq.to_string challenge) in
    let response_int  =  (G.Zq.to_string response) in
    Printf.printf "\n";
    Printf.printf "Challenge : %s\n" challenge_int;
    Printf.printf "Response  : %s\n" response_int;

    let challenge_hex =  (Z.format "%x" (Z.of_string (G.Zq.to_string challenge))) in
    let response_hex  =  (Z.format "%x" (Z.of_string (G.Zq.to_string response))) in
    Printf.printf "Challenge (hex) : %s\n" challenge_hex;
    Printf.printf "Response  (hex) : %s\n" response_hex;

    let credential = G.to_string ballot.credential in
    Printf.printf "Credential: %s\n" credential;

    (* TODO: parse credential to a group element *)
    let credential = G.of_string credential in

    let commitment = G.(
     (g **~ response) *~ (credential **~ challenge)
    ) in
     let _ = s_hash in
     let () = Printf.printf "Commitment (ou A?) : %s\n" (G.to_string commitment) in
     let make_sig_prefix hash = "sig|" ^ hash ^ "|" in
     let prefix = make_sig_prefix s_hash in
     let () = Printf.printf "hashedString : %s\n" (prefix ^ G.to_string commitment) in
     (*let x = prefix ^ map_and_concat_with_commas to_string [| commitment |] in*)
    
     let _aa = Ed25519_copy.hash prefix [| (Ed25519_copy.of_string (G.to_string commitment)) |] in
     let recomputedChallenge = G.hash prefix [| commitment |] in
     let () = Printf.printf "recomputedChallenge : %s\n"
       (Z.format "%x" (Z.of_string (G.Zq.to_string recomputedChallenge))) in

    let hashedString =
      Cryptokit.(
      "1"
      |> hash_string (Hash.sha256 ())
      |> transform_string (Hexa.encode ())
    ) in
    let () = Printf.printf "hashedString : %s\n" hashedString
    (* hashedString : 6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7875b4b *)

    in

    let hh = Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) "1" in
    let h2 = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) hh in
    let () = Printf.printf "Test hash : %s\n" hh in
    let () = Printf.printf "Test h2   : %s\n" h2 in

    let () = Printf.printf "Test h2   : %s\n" h2 in
    ()
  | None -> ()

(* Program setup *)
let setup =
  let doc = "A serialized ballot." in
  let ballot =
    let docv = "BALLOT" in
    Arg.(
      value & opt (some string) None & info [ "c"; "count" ] ~docv ~doc)
  in
  Term.(const main $ ballot)
let info =
  Cmd.info "belenios-minilib" ~version:"0.1" ~doc:"" ~man: []

let () = Cmd.eval (Cmd.v info setup) |> exit
