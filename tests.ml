open Client
module A = Array
let format = Printf.sprintf

let begin_transcription name cl =
  let finish = ref false in
  let transcript = open_out ("transcripts/" ^ name ^ ".xml") in
  let transcribe str = output_string transcript str; output_char transcript '\n'; flush transcript in
  let transcriber () =
    while !finish = false do
      cl#spill |> function
      | Error e -> begin
          transcribe e;
          finish := true;
        end
      | Ok xml -> let open Xml.P in match xml with
        | Raw.Branch ((pre,"message",attrs),ch) ->
            let xml' = Raw.Branch ((pre,"message",attrs),[ Raw.Text "[message body]" ])
            in transcribe (Xml.P.Raw.to_string xml')
        | _ -> transcribe (Xml.P.Raw.to_string xml)
    done;
    close_out transcript;
  in
  (finish, Thread.create transcriber ())

let rec select_random_all n i =
  let r = Random.int n in
  if r=i then select_random_all n i
         else r

(* 445 chars *)
let lipsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\nUt enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

let n_clients = 10
let n_msgs_per_client = 25
let prefix = format "m%d/c%d/" n_msgs_per_client n_clients

type clscribe = { cl : client; scriber : Thread.t; fin : bool ref }

let () =
  (* Synchronously initialise clients and scribers *)
  let clients = A.init n_clients (fun i ->
    let cl = new client (string_of_int i) "ptii.proj" in
    cl#handshake;
    let fin, scriber =
      begin_transcription (prefix ^ string_of_int i) cl
    in
    { cl; scriber; fin }
  ) in

  let select_for = select_random_all n_clients in

  print_endline "Connected and handshaked. Beginning...";

  (* Asynchronously send/receive *)
  let doers = A.mapi (fun i {cl; fin} ->
    Thread.create (fun () ->
      let outstanding_messages = ref n_msgs_per_client in
      while !fin = false && !outstanding_messages > 0 do
        let trg = string_of_int (select_for i) in
        cl#message_t trg lipsum;
        decr outstanding_messages;
      done
    ) ()
  ) clients in
  A.iter Thread.join doers;

  Thread.delay 3.0; (* Wait for messages to deliver etc *)

  print_endline "Finished business. Disconnecting...";

  (* Synchronously disconnect *)
  A.iter (fun {cl} -> cl#disconnect) clients

(*
  Idea is:
  all connect
  all handshake
  fence
  time
  all send/recv
  time
  fence
  all disconnect
*)
