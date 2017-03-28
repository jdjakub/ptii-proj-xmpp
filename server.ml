let format = Printf.sprintf
let pair = fun x y -> (x,y)

module Dispatch = struct
  module M = Map.Make (struct type t = string let compare = compare end) (* jid -> fwd_queue *)

  let queues = ref M.empty

  let qs_lock = Mutex.create ()

  let fmt = Printf.sprintf
  let prn_lock = Mutex.create ()
  let print str =
    Mutex.lock prn_lock;
      print_endline str;
    Mutex.unlock prn_lock

  let client_connected name =
    let q = Queue.create () in
    let q_mon = Mutex.create () in
    let avail = Condition.create () in
    let fin = ref false in
    Mutex.lock q_mon;
      queues := M.add name (q,q_mon,avail,fin) !queues;
    Mutex.unlock q_mon; print (fmt "Client connected: %s" name);
    (q,q_mon,avail,fin)

  let client_disconnected name =
    try
      let (_,_,avail,fin) = M.find name !queues in
      fin := true; Condition.signal avail
    with Not_found -> ();
    Mutex.lock qs_lock;
      queues := M.remove name !queues;
    Mutex.unlock qs_lock; print (fmt "Client disconnected: %s" name)

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
      (Mutex.unlock mon; print "Interrupted"; None)
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
    with Not_found -> () (*print (fmt "Dropped %s: %d" name work )*) (* Drop the packet *)

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

let sv_start () =
  let per_client from_ie to_ie =
    let respond str = print_endline ("[OUT]: " ^ str); output_string to_ie str; flush to_ie in
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

      Xmpp.Roster.load_from_storage user >>= fun _ ->

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

      Xmpp.Roster.get user >>= fun roster ->
      let items = Xmpp.Roster.R.bindings roster in
      let xitems = List.map (fun (_,item) -> Xmpp.Roster.to_xml item) items in
      respond_tree Raw.(xml_n "iq" [ "type", "result"; "id", req_id ] [
        xml Xmpp.jroster "query" [] xitems
      ]); expect P.tree >>| X.from_raw >>=
        Xml.Check.(tag "presence" *> orig) >>= function
      | Raw.Text _ -> Error "Presence: no children"
      | Raw.Branch (_,chs) ->

      let notify_subs stanza =
        List.iter (fun (_,{ Xmpp.Roster.jid; name; recv_ok; send_ok }) ->
          if send_ok then Dispatch.dispatch jid stanza else () ) items;
      in

      respond_tree Raw.(xml_n "presence" [ "from", raw_jid; "to", jid ] chs);

      (* Notify all subscribers that X is online *)
      notify_subs Raw.(xml_n "presence" [ "from", raw_jid ] chs);

      let work_queue = Dispatch.client_connected raw_jid in
      let stream_lock = Mutex.create () in
      let worker = Thread.create (fun workq ->
        let finish = ref false in
        while !finish = false do
          match Dispatch.dequeue_work workq with
          | Some xml ->
            Mutex.lock stream_lock;
              respond_tree xml;
            Mutex.unlock stream_lock
          | None -> finish := true
        done
      ) work_queue in

      let handle_iq = function
        | _ -> Error (Raw.(xml_n "error" [ "type", "cancel" ] [
          xml Xmpp.stanzas "feature-not-implemented" [] []
        ]))
      in
      let handle_presence = Xml.Check.(
        (attv "type" "unavailable" >>= fun _ ->
          (* Notify all subscribers that X is offline *)
          orig >>= fun pres -> notify_subs pres; pure true
        )
        <|> pure false
      )
      in
      let handle_message =
        Xml.Check.(attr "to" >>= fun recipient ->
          attr "type" >>= fun msg_type ->
          orig >>= fun (Raw.Branch ((_,_,_),chs)) ->
            pure (recipient, Raw.(xml ("","jabber:client") "message"
              [ "to", recipient; "from", raw_jid; "type", msg_type ] chs))
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
              Dispatch.print ("[FWD] " ^ Raw.to_string xml);
              Dispatch.dispatch recip xml; Ok ()
          )
        ) |> function
          | Ok _ -> ()
          | Error str -> res := Error str; finished := true
      done;
      Dispatch.client_disconnected user; (* remove dispatch access as soon as possible *)
      Thread.join worker;
      !res
    ) |> function
    | Ok _ -> print_endline "[SUCCESS]"; (* respond "</stream:stream>" *)
    | Error err -> respond "</stream:stream>"; failwith err;

  in
  Unix.(establish_server per_client (ADDR_INET (inet_addr_loopback,5222)))

let () = sv_start ()
