open Client
module A = Array

let rec select_random_all n i =
  let r = Random.int n in
  if r=i then select_random_all n i
         else r

(* 445 chars *)
let lipsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\nUt enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

let n_clients = 10
let n_msgs_per_client = 30

let () =
  (* Synchronously initialise *)
  let clients = A.init n_clients (fun i -> new client (string_of_int i) "ptii.proj") in
  A.iter (fun cl -> cl#handshake; ()) clients;

  let select_for = select_random_all n_clients in

  print_endline "Connected and handshaked. Beginning...";

  clients.(1)#message_t ~time:true "0" "Initial time message";

  let scribers = A.mapi (fun i cl ->
    let src = string_of_int i in
    begin_transcription ("m30/c50/" ^ src) cl )
  clients in

  (* Asynchronously send/receive *)
  let doers = A.mapi (fun i (cl : client) ->
    let finished = fst (scribers.(i)) in
    Thread.create (fun () ->
      let outstanding_messages = ref n_msgs_per_client in
      while !finished = false && !outstanding_messages > 0 do
        let trg = string_of_int (select_for i) in
        cl#message_t trg lipsum;
        decr outstanding_messages;
      done
    ) ()
  ) clients in
  A.iter Thread.join doers;

  clients.(1)#message_t ~time:true "0" "Final time message";

  print_endline "Finished business. Disconnecting...";

  (* Synchronously disconnect *)
  A.iter (fun cl -> cl#disconnect) clients

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
