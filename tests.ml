open Client
module A = Array

let rec select_random_all n i =
  let r = Random.int n in
  if r=i then select_random_all n i
         else r

let () =
  let n_clients = 10 in
  (* Synchronously initialise *)
  let clients = A.init n_clients (fun i -> new client (string_of_int i) "ptii.proj") in
  A.iter (fun cl -> cl#handshake; ()) clients;

  let select_for = select_random_all n_clients in

  clients.(0)#message_t ~time:true "9" "Initial time message";

  (* Asynchronously send/receive *)
  let threads = A.init n_clients (fun i ->
    let src = string_of_int i in
    let cl = clients.(i) in
    with_client src clients.(i) (fun finished ->
      let outstanding_messages = ref 10 in
      while !finished = false && !outstanding_messages != 0 do
        let trg = string_of_int (select_for i) in
        cl#message_t trg "Hello, how are you today?";
        decr outstanding_messages;
      done
    )
  ) in
  A.iter Thread.join threads;

  clients.(0)#message_t ~time:true "9" "Final time message";

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
