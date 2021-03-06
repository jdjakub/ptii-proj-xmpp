let format = Printf.sprintf
let pair = fun x y -> (x,y)

module Dispatch = struct
  module M = Map.Make (struct type t = string let compare = compare end) (* jid -> fwd_queue *)

  let queues = ref M.empty

  let qs_lock = Mutex.create ()

  let fmt = Printf.sprintf
  let prn_lock = Mutex.create ()

  let display fn =
    Mutex.lock prn_lock;
      fn ();
    Mutex.unlock prn_lock

  let all_online () =
    Mutex.lock qs_lock;
      let names = List.map fst (M.bindings !queues) in
    Mutex.unlock qs_lock;
    (*display (fun _ ->
      print_endline "All online:";
      List.iter print_endline names
    );*)
    names

  let client_connected name =
    let q = Queue.create () in
    let q_mon = Mutex.create () in
    let avail = Condition.create () in
    let fin = ref false in
    Mutex.lock qs_lock;
      queues := M.add name (q,q_mon,avail,fin) !queues;
      (* let l = M.bindings !queues in *)
    Mutex.unlock qs_lock;
    (*display (fun _ ->
      print_endline (fmt "Client connected: %s; %d connected clients:" name (List.length l));
      List.iter (fun (k,_) -> print_endline k) l
    );*)
    (q,q_mon,avail,fin)

  let client_disconnected name =
    ( try
      let (_,_,avail,fin) = M.find name !queues in
      fin := true; Condition.signal avail
    with Not_found -> () );
    Mutex.lock qs_lock;
      queues := M.remove name !queues;
    Mutex.unlock qs_lock; print_endline (fmt "Client disconnected: %s" name)

  (*
    worker thread *atomically dequeues* (blocking if empty)
    push thread *atomically enqueues*
  *)

  let dequeue_work (q,mon,avail,fin) =
    Mutex.lock mon;
    while Queue.length q = 0 && !fin = false do
      Condition.wait avail mon;
    done;
    if !fin = true then
      (Mutex.unlock mon; (*print_endline "Interrupted";*) None)
    else let work = Queue.take q in
      (Mutex.unlock mon; Some work)

  let dispatch name work =
    try
      let (q,q_mon,avail,_) = M.find name !queues in
      Mutex.lock q_mon;
        Queue.add work q;
        Condition.signal avail;
      Mutex.unlock q_mon;
      (*print (fmt "%s: %d" name work)*)
    with Not_found -> () (*print_endline (fmt "Dropped work for %s" name) (* Drop the packet *)*)

end

module Driver = struct
  open Unix

  (* Version of Unix.establish_server, spawning threads instead of processes *)
  let establish_server per_client listen_addr backlog =
    let s_listen = socket PF_INET SOCK_STREAM 0 in
    setsockopt s_listen SO_REUSEADDR true; (* required for quick restart of server *)
    bind s_listen listen_addr;
    listen s_listen backlog;
    while true do
      let s_client, _ = accept s_listen in
      let inch = in_channel_of_descr s_client in
      let ouch = out_channel_of_descr s_client in
      let server_fun_with_cleanup (inc,outc) =
        let result = per_client inc outc in
        close s_client;
        match result with
        | Ok _ -> ()
        | Error err -> failwith err
      in
      Thread.create server_fun_with_cleanup (inch,ouch);
    done
end

open Rresult

let (<|>) ex ey = match ex with
  | Ok x -> Ok x
  | Error e -> ey

module A = Angstrom
module X = Xml
module P = X.P
module Raw = P.Raw

let plain_auth_extract str =
  let open A in
  let nul = char '\x00' in
  let p = lift2 pair
    (nul *> take_till (function | '\x00' -> true | _ -> false))
    (nul *> take_till (fun _ -> false))
  in parse_only p (`String str)

let n_msgs = ref 0
let m_lock = Mutex.create ()
let inc_count () =
  Mutex.lock m_lock;
    incr n_msgs;
  Mutex.unlock m_lock

let sv_start () =
  let per_client from_ie to_ie =
    let respond str = (*print_endline ("[OUT]: " ^ str);*) output_string to_ie str; flush to_ie in
    let respond_tree xml = respond (Raw.to_string xml) in
    let expect = Xml.buffered_expect from_ie in
    let stream_handshake id =
      expect A.(P.xml_decl *> P.tag_open) >>| X.from_raw >>=
        Xml.Check.(qtag Xmpp.jstream "stream" *> attr "to") >>= fun my_addr ->
      let response = Raw.(to_string_open (xml_d Xmpp.jstream "stream" [
        "xmlns", snd Xmpp.jclient;
        "version", "1.0"; "from", my_addr; "id", id;
      ] []))
      in respond response; Ok (my_addr, id)
    in
    ( stream_handshake "random-id" >>= fun (my_addr,id) ->
      respond_tree Raw.(xml Xmpp.jstream "features" [] [
        xml Xmpp.sasl "mechanisms" [] [
          xml_n "mechanism" [] [ text "PLAIN" ];
          xml_n "required" [] []
        ]
      ]);
      expect P.tree >>| X.from_raw >>=
        Xml.Check.(qtag Xmpp.sasl "auth" *> attv "mechanism" "PLAIN" *> child) >>=
        Xml.Check.text >>= fun garbled ->

      let ungarbled = B64.decode garbled in
      plain_auth_extract ungarbled >>= fun (user,pass) ->
      print_endline ("USERNAME: " ^ user);
      print_endline ("PASSWORD: " ^ pass);
      respond_tree Raw.(xml Xmpp.sasl "success" [] []);

      Xmpp.Roster.load_from_storage user; (* ignore not found *)

      stream_handshake "different-id" >>= fun (my_addr,id) ->
      respond_tree Raw.(xml Xmpp.jstream "features" [] [
        xml Xmpp.bind "bind" [] [ xml_n "required" [] [] ]
      ]);

      expect P.tree >>| X.from_raw >>= Xmpp.Stanza.Iq.of_xml >>=
      fun { req_id; iq_type=(Set tag_bind) } ->
        (tag_bind |> Xml.Check.(qtag Xmpp.bind "bind" *> child)) >>=
      Xml.Check.(tag "resource" *> child) >>= Xml.Check.text >>=
      fun resource ->

      let raw_jid = user ^ "@" ^ my_addr in
      let jid = raw_jid ^ "/" ^ resource in
      respond_tree Raw.(xml_n "iq" [ "id", req_id; "type", "result" ] [
        xml Xmpp.bind "bind" [] [
          xml_n "jid" [] [ text jid ]
        ]
      ]);

      (* Assuming it was the iq type=set { session } thing, which Psi does *)
      expect P.tree >>| X.from_raw >>= Xmpp.Stanza.Iq.of_xml >>=
      fun { req_id; iq_type=(Set query) } ->
        (query |> Xml.Check.qtag Xmpp.session "session") >>= fun _ ->

      respond_tree Raw.(xml_n "iq" [ "type", "result"; "id", req_id ] []);
      (* Assuming iq type=get { jroster:query } *)
      expect P.tree >>| X.from_raw >>= Xmpp.Stanza.Iq.of_xml >>=
      fun { req_id; iq_type=(Get query) } ->
        (query |> Xml.Check.qtag Xmpp.jroster "query") >>= fun _ ->

      let items_opt = Xmpp.Roster.get user in
      let items = match items_opt with
        | Some roster -> Xmpp.Roster.R.bindings roster
        | None        -> []
      in
      let xitems = List.map (fun (_,item) -> Xmpp.Roster.to_xml item) items in
      respond_tree Raw.(xml_n "iq" [ "type", "result"; "id", req_id ] [
        xml Xmpp.jroster "query" [] xitems
      ]);

      expect P.tree >>| X.from_raw >>=
        Xml.Check.(tag "presence" *> orig) >>= function
          | Raw.Text _ -> Error "Presence: no children"
          | Raw.Branch (_,chs) ->

      let notify_subs stanza =
        let subs = match Xmpp.Roster.get_subs_of user with
          | None -> Dispatch.all_online () (* By default, everyone's a subscriber! *)
          | Some items -> List.map (fun (_,item) -> item.Xmpp.Roster.jid) items
        in
        List.iter (fun jid ->
          (*print_endline ("Considering " ^ jid);*)
          if jid <> raw_jid then (
            (*print_endline "Notifying of presence";*)
            Dispatch.dispatch jid stanza )
          else
            () (*print_endline "Not notifying. Already did so."*)
        ) subs;
      in

      respond_tree Raw.(xml_n "presence" [ "from", raw_jid; "to", jid ] chs);

      (* Notify all subscribers that the client is online *)
      notify_subs Raw.(xml_n "presence" [ "from", raw_jid ] chs);

      let work_queue = Dispatch.client_connected raw_jid in
      let stream_lock = Mutex.create () in
      let worker = Thread.create (fun workq ->
        let finish = ref false in
        while !finish = false do
          match Dispatch.dequeue_work workq with
          | Some xml -> (
            Mutex.lock stream_lock;
              respond_tree xml;
              (*print_endline (Raw.to_string xml);*)
            Mutex.unlock stream_lock;
            match xml with
            | Raw.Branch ((_,"message",_),_) -> inc_count ()
            | _ -> ()
          )
          | None -> finish := true
        done
      ) work_queue in

      let handle_iq = function
        | _ -> Error (Raw.(xml_n "error" [ "type", "cancel" ] [
          xml Xmpp.stanzas "feature-not-implemented" [] []
        ]))
      in
      let handle_presence = Xml.Check.(
        (attv "type" "unavailable" *> (attr "from" *> pure false <|>
            (* Notify all subscribers that X is offline *)
            (orig >>= fun pres ->
              notify_subs (Raw.with_attrs [ "from", raw_jid ] pres);
              pure true)
            (* no "from" attr means it is legit and
            should be fwd'd with "from" attr *)
          )
        )
      )
      in
      let handle_message =
        Xml.Check.(attr "to" >>= fun recipient ->
          attr "type" >>= fun msg_type ->
          orig >>= fun (Raw.Branch ((_,_,_),chs)) ->
            let attrs = [ "to", recipient; "from", raw_jid; "type", msg_type ] in
            pure (recipient, Raw.(xml ("","jabber:client") "message" attrs chs))
        )
      in
      let finished = ref false in
      let res = ref (Ok ()) in
      let self_dispatch x = Dispatch.dispatch raw_jid x; Ok () in
      while !finished = false do
        (expect P.tree >>| X.from_raw >>= fun raw ->
          ((raw |> Xmpp.Stanza.Iq.of_xml) >>= fun { req_id; iq_type } ->
            let (ret_type, body) = match handle_iq iq_type with
              | Ok inner -> ("result", inner)
              | Error err -> ("error", err)
            in
            Ok (let respns =
              Raw.(xml_n "iq" [ "type", ret_type; "id", req_id ] [ body ])
              in self_dispatch respns)
          ) <|>
          ((raw |> Xml.Check.tag "presence") >>= fun pres ->
            handle_presence pres >>| fun quit ->
              finished := quit;
              Ok ()
          ) <|>
          ((raw |> Xml.Check.tag "message") >>= fun to_addr ->
            handle_message raw >>| fun (recip,xml) ->
              (*print_endline ("[FWD] " ^ Raw.to_string xml);*)
              Dispatch.dispatch recip xml; Ok ()
          )
        ) |> function
          | Ok _ -> ()
          | Error str -> res := Error str; finished := true
      done;
      Dispatch.client_disconnected raw_jid; (* remove dispatch access as soon as possible *)
      Thread.join worker;
      !res
    ) |> function
    | Ok _ -> (*print_endline "[SUCCESS]";*) Ok () (* respond "</stream:stream>" *)
    | Error err -> respond "</stream:stream>"; Error err

  in
  Driver.establish_server per_client Unix.(ADDR_INET (inet_addr_loopback,5222)) 5

let sampler () =
  let time () = (Unix.gettimeofday (), Sys.time ()) in
  let t_prev = ref (time ()) in
  let prev_msgs = ref 0 in
  let t_ideal_sleep = 5.0 in

  while true do
    let p = !prev_msgs in
    let n = !n_msgs in
    let t_now = time () in
    prev_msgs := n;
    let delta_n = n - p in
    let delta_unix_s = fst t_now -. fst !t_prev in
    let delta_sys_s = snd t_now -. snd !t_prev in
    t_prev := t_now;
    let throughp_unix = float_of_int delta_n /. delta_unix_s in
    let throughp_sys = float_of_int delta_n /. delta_sys_s in
    print_endline (format "DISPATCHED %d msgs in (u,d): (%f,%f)s; rate (%f,%f)msg/s"
                    delta_n delta_unix_s delta_sys_s throughp_unix throughp_sys);
    let t_again = time () in
    let t_spent = fst t_again -. fst t_now in
    let t_sleep = t_ideal_sleep -. t_spent in
    Thread.delay t_sleep;
  done

let () =
  Thread.create sampler ();
  sv_start ()
