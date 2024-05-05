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

    let (a, b) = (Ed25519_copy.to_coordinates Ed25519_copy.g) in
    Ed25519_copy.F.(
      Printf.printf "G a: %s\n" (to_string a);
      Printf.printf "G b: %s\n" (to_string b);
    );

    let ed_credential = Ed25519_copy.of_string (G.to_string ballot.credential) in
    let (a, b) = (Ed25519_copy.to_coordinates ed_credential) in
    Ed25519_copy.F.(
      Printf.printf "Credential a: %s\n" (to_string a);
      Printf.printf "Credential b: %s\n" (to_string b);

      Printf.printf "Credential a (hex): %s\n" (Z.format "%x" (Z.of_string (to_string a)));
      Printf.printf "Credential b (hex): %s\n" (Z.format "%x" (Z.of_string (to_string b)));

      Printf.printf "g ** response: %s\n" G.(to_string (g **~ response));
      Printf.printf "credential ** challenge: %s\n" G.(to_string (credential **~ challenge));
    );

    let commitment = G.(
     (g **~ response) *~ (credential **~ challenge)
    ) in
     let _ = s_hash in
     let () = Printf.printf "Commitment (ou A?) : %s\n" (G.to_string commitment) in
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
