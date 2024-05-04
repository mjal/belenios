open Cmdliner
open Belenios_core

(*
let g = Ed25519_pure.g;;
let challenge = "1337099679088420191945735243092362180482631661342475760186294036356510222127";;
let response = "584715790211560003328841421218527705726927443831262760529448472738263625037";;
let credential = "6d976b5f243bfada5f744b2dfd9a0a19784c89221c981e686c1aeca0dcc04ea2";;
*)

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

(* Read the file ballot.json from the filesystem *)
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
      let challenge =  (G.Zq.to_string ballot_sig.s_proof.challenge) in
      let response  =  (G.Zq.to_string ballot_sig.s_proof.response) in
      let hash      =  ballot_sig.s_hash in
      Printf.printf "\n";
      Printf.printf "Hash      : %s\n" hash;
      Printf.printf "Challenge : %s\n" challenge;
      Printf.printf "Response  : %s\n" response;
      Printf.printf "Credential: %s\n" credential;

      Printf.printf "Ballot: %s\n" ballot_str;
      Printf.printf "Ballot hash: %s\n" (Belenios_core.Common_types.Hash.(
        unwrap (hash_string ballot_str)
      ));

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
      ()

  | None -> Printf.printf "Signature is not present.\n";

  (*let signature = G.to_string b.signature in*)
  Printf.printf "Here is your credential: %s\n" credential;;

  (*
  let ballot = int_of_string ballot in
  let ballot = Belenios_core.Ballot.of_int ballot in
  let ballot = Belenios_core.Ballot.to_string ballot in
  *)

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
