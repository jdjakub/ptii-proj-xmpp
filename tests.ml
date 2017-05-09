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

let select_random_all n () = Random.int n

(* 445 chars *)
let lipsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\nUt enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

let n_senders = 5
let n_recvers = n_senders

let consumer (cl,i) =
  let finish = ref false in
  while !finish = false do
    cl#spill |> function
    | Error e -> begin
        print_endline (format "Error in client #%d: %s" i e);
      end
    | _ -> ()
  done

let () =
  (* Synchronously initialise clients and consumers *)
  let clients = A.init (n_senders + n_recvers) (fun i ->
    let cl = new client (string_of_int i) "ptii.proj" in
    cl#handshake;
    Thread.create consumer (cl,i); (* Must come after handshake *)
    cl
  ) in

  at_exit (fun () ->
    A.iter (fun cl -> cl#disconnect) clients
  );

  let select_trg = select_random_all n_recvers in

  Thread.delay 5.0;

  print_endline "Connected and handshaked. Beginning...";

  (* Asynchronously send *)
  let threads = A.mapi (fun i (cl:client) ->
    if i >= n_recvers then
      Some (Thread.create (fun () ->
        while true do
          let trg = string_of_int (select_trg ()) in
          cl#message_t trg lipsum;
        done
      ) ())
    else None
  ) clients
  in

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
