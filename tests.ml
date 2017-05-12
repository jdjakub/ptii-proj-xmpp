open Client
module A = Array
let format = Printf.sprintf

let select_random_all n () = Random.int n

(* 445 chars *)
let lipsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\nUt enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

let n_senders = 50
let n_recvers = n_senders

module Raw = Xml.P.Raw

let consumer (cl,i) =
  let finish = ref false in
  while !finish = false do
    cl#spill |> function
    | Error e -> begin
        print_endline (format "Error in client #%d: %s" i e);
        finish := true
      end
    | Ok (Xml.P.Raw.Branch ((_,"stream",_),_)) -> begin
        print_endline "Got closing </stream:stream>";
        finish := true
      end
    | _ -> ()
  done

let () =
  (* Synchronously initialise clients and consumers *)
  let clients = A.init (n_senders + n_recvers) (fun i ->
    let cl = new client (string_of_int i) "ptii.proj" in
    print_endline (format "Created client %d" i);
    cl#handshake;
    print_endline (format "Client %d handshaked" i);
    Thread.create consumer (cl,i); (* Must come after handshake *)
    cl
  ) in

  let select_trg = select_random_all n_recvers in

  print_endline "Connected and handshaked. Beginning...";
  Thread.delay 1.0;
  print_endline "Three";
  Thread.delay 1.0;
  print_endline "Two";
  Thread.delay 1.0;
  print_endline "One";
  Thread.delay 1.0;
  print_endline "Go.";

  let stop = ref false in

  (* Asynchronously send *)
  let threads = A.mapi (fun i cl ->
    if i >= n_recvers then
      Some (Thread.create (fun () ->
        while !stop = false do
          let trg = string_of_int (select_trg ()) in
          cl#message_t trg lipsum;
        done
      ) ())
    else None
  ) clients
  in

  at_exit (fun () ->
    stop := true;
    (* How crazy is this?! *)
    A.iter (function | Some t -> Thread.join t | None -> ()) threads;
    A.iter (fun cl -> cl#disconnect) clients
  );

  (* LOOP TO INFINITY AND BEYOND *)
  A.iter (function | Some t -> Thread.join t | None -> ()) threads;

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
