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
  let credential = G.to_string ballot.credential in
  match ballot.signature with
  | Some ballot_sig ->
      (*
      let challenge =  (G.Zq.to_string ballot_sig.s_proof.challenge) in
      let response  =  (G.Zq.to_string ballot_sig.s_proof.response) in
      Printf.printf "Challenge : %s\n" challenge;
      Printf.printf "Response  : %s\n" response;
      *)
      let challenge_hex =  (Z.format "%x" (Z.of_string (G.Zq.to_string ballot_sig.s_proof.challenge))) in
      let response_hex  =  (Z.format "%x" (Z.of_string (G.Zq.to_string ballot_sig.s_proof.response))) in
      Printf.printf "Challenge (hex) : %s\n" challenge_hex;
      Printf.printf "Response  (hex) : %s\n" response_hex;
      Printf.printf "Credential: %s\n" credential;

      match ballot.signature with
      | Some { s_hash; s_proof = { challenge; response } } ->
         (* TODO: parse credential to a group element *)
         let credential = G.of_string credential in
         let commitment = G.(
          (g **~ response) *~ (credential **~ challenge)
         ) in
          let _ = s_hash in
          let () = Printf.printf "Commitment : %s\n" (G.to_string commitment) in
         ()
         (*
           let prefix = make_sig_prefix s_hash in
           Zq.(challenge =% G.hash prefix [| commitment |])
         *)
      | None -> ();

      (*
      match ballot_sig.s_hash with
      | 
      (*let g = Ed25519_pure.g in*)
      let commitment = Ed25519_pure.(
        g **~ (ballot_sig.s_proof.response)
      ) in
      *)

      (* *~ (ballot.credential **~ (G.Zq.of_string challenge_hex)) in *)

      (*
       s_hash = expected_hash && G.check credential
       &&
       let commitment = (g **~ response) *~ (credential **~ challenge) in
       let prefix = make_sig_prefix s_hash in
       Zq.(challenge =% G.hash prefix [| commitment |])
      *)

      (*
      let hash      =  ballot_sig.s_hash in
      Printf.printf "Ballot: %s\n" ballot_str;
      Printf.printf "Ballot hash: %s\n" (Belenios_core.Common_types.Hash.(
        unwrap (hash_string ballot_str)
      ));
      *)

      (*
      let ballot_without_signature : (G.t, G.Zq.t) Belenios_v1.Serializable_t.ballot =
        {
          election_uuid = ballot.election_uuid;
          election_hash = ballot.election_hash;
          credential = ballot.credential;
          answers = ballot.answers;
          signature = None;
        }
      in

      let serialized_ballot = 
        Common.(
          (Belenios_v1.Serializable_j.string_of_ballot (swrite G.to_string) (swrite G.Zq.to_string)
             ballot_without_signature)
        )
      in
      let s_hash  = Belenios_core.Common.sha256_b64 serialized_ballot in
      let s_hash2 = Belenios_core.Common.sha256_hex serialized_ballot in
      Printf.printf "serialized_ballot: %s\n" serialized_ballot;
      Printf.printf "s_hash: %s\n" s_hash;
      Printf.printf "s_hash2: %s\n" s_hash2;
      *)

      (*
        (string_of_ballot (swrite G.to_string) (swrite G.Zq.to_string)
           ballot_without_signature)
       *)

      (*
      let election_uuid = ballot.election_uuid in
      let election_hash = ballot.election_hash in
      *)
      (* 
      let ballot_without_signature : ('a, 'b) Serializable_j.ballot =
        { election_uuid; election_hash; credential; answers; signature = None };
      let credential = G.to_string ballot.credential in
      let ballot_without_signature = { election_uuid; election_hash; credential; answers = ballot.answers; signature = None } in
      let ballot_without_signature_str = "" in
      *)

        (* let ballot = Belenios_v1.Serializable_j.ballot_of_string *)
        (*   (Common.sread G.of_string) (Common.sread G.Zq.of_string) ballot_str in *)
      (*Printf.printf "Ballot without sig  : %s\n" ballot_without_signature_str in*)
      (* Printf.printf "Ballot without sig hash : %s\n" "TODO"; *)

  | None -> Printf.printf "Signature is not present.\n";
  (*let signature = G.to_string b.signature in*)
  ()

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
