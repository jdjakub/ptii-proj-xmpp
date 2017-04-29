open Client
module A = Array

let () =
  let n = 10 in
  (* Synchronously initialise *)
  let clients = A.init n (fun i -> new client (string_of_int i) "ptii.proj") in
  A.iter (fun cl -> cl#handshake; ()) clients;

  clients.(0)#message_t ~time:true "9" "Initial time message";

  (* Asynchronously send/receive *)
  let threads = A.init n (fun i ->
    let src = string_of_int i in
    let trg = string_of_int ((i+1) mod 10) in
    let cl = clients.(i) in
    with_client src clients.(i) (fun finished ->
      cl#message_t trg "Hello, how are you today?";
    )
  ) in
  A.iter Thread.join threads;

  clients.(0)#message_t ~time:true "9" "Final time message";

  (* Synchronously disconnect *)
  A.iter (fun cl -> cl#disconnect) clients

(* Must be able to wait till: all handshaked, all finished conversing *)
(* Semaphore counting down? 10 9 8 7 ... 3 2 1 0 all clear, finished handshaking *)
(* Well, maybe not a semaphore. Block after dec, until 0. Condition? *)

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
