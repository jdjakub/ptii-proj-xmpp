let format = Printf.sprintf
let pair = fun x y -> (x,y)

module Xml = struct
  type qname = string * string
  type lang = string

  module A = Map.Make(struct
    type t = qname
    let compare = compare
  end)

  module N = Map.Make(struct
    type t = string
    let compare = compare
  end)

  module P = struct
    open Angstrom

    let is_space = function
      | ' ' | '\t' | '\r' | '\n' -> true | _ -> false

    let is_ident = function
      | 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

    let spaces = many (satisfy is_space)

    let lex p = spaces *> p

    let tok_langle = lex (char '<')
    let tok_rangle = lex (char '>')
    let tok_leaf = lex (string "/>")
    let tok_close = lex (string "</")
    let tok_colon = lex (char ':')
    let tok_equ = lex (char '=')
    let tok_ident = lex (take_while1 is_ident)

    (* Does not handle escapes! *)
    let tok_string = (lex (char '"') *> take_till (fun c -> c = '"') <* char '"')
      <|> (lex (char '\'') *> take_till (fun c -> c = '\'') <* char '\'')

    (* [namespace :] tag *)
    let qual_name =
      tok_ident >>= fun id1 ->
        (tok_colon *> tok_ident >>| fun id2 -> id1,id2) <|> return ("",id1)

    let attr_val =
      lift2 pair
        qual_name
        (tok_equ *> tok_string)

    (* <?xml version="1.0" ?> *)
    let xml_decl = lex (string "<?xml") *> many attr_val <* lex (string "?>")

    module Raw = struct
      type attr = (string * string) * string
      type xml = Text of string | Branch of (string * string * attr list) * xml list

      let attr (k,v) = (("",k),v)
      let text t = Text t
      (* Suppresses namespace definition except for default namespace *)
      let xml (pre,full) tag attrs children =
        let attrs' = List.map attr attrs in
        let attrs'' = match (pre,full) with
          | ("","") -> attrs'
          | ("",full) -> attr ("xmlns",full) :: attrs'
          | (pre,full) -> attrs'
        in
        Branch ((pre,tag,attrs''), children)

      let xml_n tag = xml ("","") tag

      (* Inserts xmlns:pre=full if applicable *)
      let xml_d (pre,full) tag attrs children =
        let attrs' = List.map attr attrs in Branch ((pre,tag,
          match (pre,full) with
          | (pre,"")   -> attrs'
          | ("",full)  -> attr ("xmlns",full) :: attrs'
          | (pre,full) -> (("xmlns",pre),full)  :: attrs'
        ), children)

      let string_of_qn = function
        | ("",name) -> name
        | (ns,name) -> ns ^ ":" ^ name

      let string_of_attrs attrs =
        let to_string (qn,value) =
            format " %s=\"%s\"" (string_of_qn qn) value (* DOES NOT SANITISE VAL!! *)
        in
        String.concat "" (List.map to_string attrs)

      let to_string_open (Branch ((ns,tag,attrs),_)) = format "<%s%s>" (string_of_qn (ns,tag)) (string_of_attrs attrs)

      let to_string_single (ns,tag,attrs) = format "<%s%s/>" (string_of_qn (ns,tag)) (string_of_attrs attrs)

      let to_string_close (Branch ((ns,tag,_),_)) = format "</%s>" (string_of_qn (ns,tag))

      let rec to_string = function
        | Text str -> str
        | (Branch ((ns,tag,attrs),children)) as xml ->
          match children with
          | [] -> to_string_single (ns,tag,attrs)
          | _  -> let interior = String.concat "" (List.map to_string children) in
                  to_string_open xml ^ interior ^ to_string_close xml
    end

     (* < tag (attr=val)* > *)
    let tag_open =
      tok_langle *> qual_name >>= fun (ns,id) ->
        lift2 (fun attrs _ -> Raw.Branch ((ns,id,attrs),[]))
          (many attr_val)
          tok_rangle

    (* </ tag > *)
    let tag_close (ns,id) =
      tok_close *> qual_name >>= fun (ns',id') ->
        if ns = ns' && id = id'
          then tok_rangle else fail (format "Expected tag %s:%s, got %s:%s" ns id ns' id')

    (* branch = </ tag > | tree branch *)
    let branch t_rec tagname = fix ( fun b_rec ->
      (tag_close tagname *> return []) <|>
        (t_rec >>= fun tree -> b_rec >>| fun trees -> tree::trees) )

    (* < tag attr_val* ( /> | > branch(tag) ) | text *)
    let tree = fix ( fun t_rec ->
      ( tok_langle *> qual_name >>= fun (ns,id) ->
              lift2 (fun attrs children -> Raw.Branch ((ns, id, attrs), children))
                (many attr_val)
                (tok_leaf *> return [] <|> tok_rangle *> branch t_rec (ns,id)) )
      <|> (take_while1 (function | '<' -> false | _ -> true) >>| Raw.text) )

  end

  type xml_node =
  | Text of lang * string
  | Xml of {
    tag    : qname;
    attr   : string -> string option;
    attr_full : qname -> string option;
    namespace : string -> string option;
    lang  : lang;
    child : xml_node list;
    orig  : P.Raw.xml;
  }

  let rec from_raw_br namespace lang = function
  | P.Raw.Text t -> Text (lang,t)
  | (P.Raw.Branch ((prefix,tag,attrs),children)) as orig ->
    let (attr_m,namespace_m,lang) = List.fold_left (fun (a,n,l) -> function
      | (("","xmlns"),value) -> (a,N.add "" value n,l)
      | (("xml","lang"),value) -> (a,n,value)
      | (("xmlns",ns),value) -> (a,N.add ns value n,l)
      | (qn,value) -> (A.add qn value a,n,l)
    ) (A.empty, N.empty, lang) attrs
    in
    let attr_full qn =
      try Some (A.find qn attr_m)
      with Not_found -> None
    in
    let attr k = attr_full ("",k) in
    let namespace ns =
      try Some (N.find ns namespace_m)
      with Not_found -> namespace ns
    in
    Xml {
      tag = (prefix,tag); attr; attr_full; namespace; lang;
      child = List.map (from_raw_br namespace lang) children;
      orig;
    }

  let from_raw xml = from_raw_br
    (function | "" -> Some "" | _ -> None)
    "en" xml

  module Check = struct
    let tag t = function
      | Xml xml ->
        let t' = snd xml.tag in
        if t' = t then Ok t else Error t'
      | Text _ -> Error ("Expected '" ^ t ^ "' element, got content instead")

    let attr k = function
      | Xml xml -> (match xml.attr k with
        | Some v -> Ok v
        | None -> Error (format "Expected %s attribute in <%s> tag" k (snd xml.tag)) )
      | Text _ -> Error "Expected XML, not content"

    let attr_opt k = function
      | Xml xml -> Ok (xml.attr k)
      | Text _ -> Error "Expected XML, not content"

    let child = function
      | Xml xml -> ( match xml.child with
        | [] -> Error (format "<%s> has no child XML" (snd xml.tag))
        | ch::_ -> Ok ch )
      | Text _ -> Error "Expected XML, not content"

    let children = function
      | Xml xml -> Ok xml.child
      | Text _ -> Error "Expected XML, not content"

    let orig = function
      | Xml xml -> Ok xml.orig
      | Text _ -> Error "Expected XML, not content"

    let text = function
      | Xml _ -> Error "Expected content, not XML"
      | Text (_,str) -> Ok str

    (* f xml *> g xml *)
    let ( *> ) f g xml = match f xml with
      | Ok _ -> g xml
      | Error e -> Error e

    let ( <* ) f g = g *> f

    let ( >*> ) f g xml =
      Rresult.R.map (g xml) (f xml)

    (* f xml >>= g xml ? requires g = fun xml v -> ... xml *)
    let ( >>= ) f g xml = match f xml with
      | Ok v -> g v xml
      | Error e -> Error e

    let ( >>| ) f g xml =
      Rresult.R.map g (f xml)

    let ( <|> ) f g xml = match f xml with
    | Ok v -> Ok v
    | Error _ -> match g xml with
      | Ok v -> Ok v
      | Error e -> Error e

    let pure v = fun _ -> Ok v
    let fail v = fun _ -> Error v

    let qtag (pre,ns) t = tag t *> fun (Xml xml) ->
      let pre' = fst xml.tag in
      if pre = "" || pre = pre' then
        match xml.namespace pre' with
        | Some ns' -> if ns = ns' then Ok t else Error "Wrong namespace"
        | None     -> Error "No such namespace"
      else Error "Wrong tag prefix"

    let attv k v = attr k >>= fun v' (Xml xml) ->
      if v = v' then Ok v else
        let t = snd xml.tag in
        let error = format "Expected <%s ... %s=\"%s\" ...>, got %s=\"%s\"" t k v k v'  in
        Error error
  end
end

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
    Mutex.lock q_mon;
      queues := M.add name (q,q_mon,avail) !queues;
    Mutex.unlock q_mon; print (fmt "Client connected: %s" name);
    (q,q_mon,avail)

  let client_disconnected name =
    Mutex.lock qs_lock;
      queues := M.remove name !queues;
    Mutex.unlock qs_lock; print (fmt "Client disconnected: %s" name)

  (*
    worker thread *atomically dequeues* (blocking if empty)
    push thread *atomically enqueues*
  *)

  let dequeue_work (q,mon,avail) =
    Mutex.lock mon;
      while Queue.length q = 0 do
        Condition.wait avail mon;
      done;
      let work = Queue.take q in
    Mutex.unlock mon;
    work

  let dispatch name work =
    try
      let (q,q_mon,avail) = M.find name !queues in
      Mutex.lock q_mon;
        Queue.add work q;
        Condition.signal avail;
      Mutex.unlock q_mon;
      (*print (fmt "%s: %d" name work)*)
    with Not_found -> () (*print (fmt "Dropped %s: %d" name work )*) (* Drop the packet *)

end

module Xmpp = struct
  let none = ("","")
  let streams = ("","urn:ietf:params:xml:ns:xmpp-streams")
  let session = ("","urn:ietf:params:xml:ns:xmpp-session")
  let stanzas = ("","urn:ietf:params:xml:ns:xmpp-stanzas")
  let sasl    = ("","urn:ietf:params:xml:ns:xmpp-sasl")
  let bind    = ("","urn:ietf:params:xml:ns:xmpp-bind")
  let jstream  = ("stream","http://etherx.jabber.org/streams")
  let jclient  = ("client","jabber:client")
  let jserver  = ("server","jabber:server")
  let jroster  = ("","jabber:iq:roster")

  module Roster = struct
    module R = Map.Make (struct type t = string let compare = compare end) (* jid -> item *)
    module M = Map.Make (struct type t = string let compare = compare end) (* jid -> roster *)

    type item = {
      jid : string;
      name : string;
      recv_ok : bool;
      send_ok : bool;
      groups : string list;
    }

    let to_xml item =
      let sub = match (item.recv_ok, item.send_ok) with
        | (false,false) -> "none"
        | (false,true) -> "from"
        | (true,false) -> "to"
        | (true,true) -> "both"
      in
      Xml.P.Raw.xml_n "item" [ "jid", item.jid; "name", item.name; "subscription", sub ] []

    module Rr = Rresult

    let item_of_xml =
      let open Xml.Check in
      tag "item" *> attr "jid" >>= fun jid ->
        attr "name" >>= fun name ->
          attr_opt "subscription" >>= ( function
          | None        -> pure (false,false)
          | Some "none" -> pure (false,false)
          | Some "to"   -> pure (true,false)
          | Some "from" -> pure (false,true)
          | Some "both" -> pure (true,true)
          | _ -> fail "Unknown subscription type" ) >>=
      fun (recv_ok,send_ok) ->
        pure { jid; name; recv_ok; send_ok; groups=[] } (* no groups *)

    type roster = item R.t

    let rosters = ref M.empty

    let get name = try Ok (M.find name !rosters)
      with Not_found -> Error ("No such roster loaded: " ^ name)

    let roster_from_in_ch in_ch =
      let open Angstrom.Buffered in
      let buf = String.make 2000 '.' in
      let rec iter = function
        | Partial next ->
            let len = input in_ch buf 0 2000 in
            let inp = String.sub buf 0 len in
            print_endline ("[ROSTER]: " ^ inp ^ "[/ROSTER]");
            if len = 0 then iter (next (`Eof))
            else iter (next (`String inp))
        | Fail (unc,strs,str) ->
            Error (format "Parse error:\n%s\n%s\n" str (String.concat "\n" strs))
        | Done (unc,result) ->
            Ok result (* THROWS AWAY UNCONSUMED!!! *)
      in Rr.R.(
      iter (parse Angstrom.(Xml.P.tree >>| Xml.from_raw)) >>= Xml.Check.children >>=
        let process rstr_res tree = rstr_res >>= fun (i,rstr) -> print_endline (string_of_int i); item_of_xml tree
          >>= fun item -> Ok (i+1, R.add item.jid item rstr)
        in
        List.fold_left process (Ok (1,R.empty)) )

    let load_from_storage name =
      try M.find name !rosters; Ok ()
      with Not_found ->
        rosters := M.add name R.empty !rosters;
        try
          let in_ch = open_in ("roster/" ^ name ^ ".xml") in
          Rr.( roster_from_in_ch in_ch >>| fun (_,r) ->
           close_in in_ch;
           rosters := M.add name r !rosters )
        with Sys_error _ -> Error ("File roster/" ^ name ^ ".xml not found")

  end

  module Stanza = struct
    type error = string

    module Iq = struct

      type query = Xml.xml_node
      type iq_type =
      | Get of query
      | Set of query
      | Result of query option
      | Error of query option * error

      type t = {
        req_id : string;
        iq_type : iq_type;
      }

      let of_xml =
      let open Xml.Check in
        tag "iq" *> attr "id" >>= fun req_id ->
          attr "type" >>= (function
          | "get" -> child >>| fun ch -> Get ch
          | "set" -> child >>| fun ch -> Set ch
          | "result" -> fun xml -> Ok (Result (Rresult.R.to_option (child xml)))
          | "error" -> fun xml -> Ok (Error (Rresult.R.to_option (child xml),"?"))
          | _ -> fail "bad-request") >>| fun iq_type -> { req_id; iq_type }

    end

    module Presence = struct

    end
  end
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

let expect buf read respond p =
  let open A.Buffered in
  let rec run = function
    | Partial next ->
        let len = read buf 0 2000 in
        let inp = String.sub buf 0 len in
        print_endline ("[IN]: " ^ inp ^ "[/IN]");
        if len = 0 then Error "Didn't get anything"
        else run (next (`String inp))
    | Fail (unc,strs,str) ->
        Error (format "Parse error:\n%s\n%s\n" str (String.concat "\n" strs))
    | Done (unc,result) ->
        Ok result (* THROWS AWAY UNCONSUMED!!! *)
  in run (parse p)

let sv_start () =
  let per_client from_ie to_ie =
    let respond str = print_endline ("[OUT]: " ^ str); output_string to_ie str; flush to_ie in
    let respond_tree xml = respond (Raw.to_string xml) in
    let buf = String.make 2000 '.' in
    let expect p = expect buf (input from_ie) respond p in
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

      let jid = format "%s@%s/%s" user my_addr resource in
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
      let items = List.map (fun (_,item) -> Xmpp.Roster.to_xml item) items in
      respond_tree Raw.(xml_n "iq" [ "type", "result"; "id", req_id ] [
        xml Xmpp.jroster "query" [] items
      ]); expect P.tree >>| X.from_raw >>=
        Xml.Check.(tag "presence" *> orig) >>= function
      | Raw.Text _ -> Error "Presence: no children"
      | Raw.Branch (_,chs) ->

      respond_tree Raw.(xml_n "presence" [ "from", jid; "to", jid ] chs);

      let work_queue = Dispatch.client_connected user in
      let stream_lock = Mutex.create () in
      let worker = Thread.create (fun workq ->
        while true do
          match Dispatch.dequeue_work workq with
          | xml ->
            Mutex.lock stream_lock;
              respond_tree xml;
            Mutex.unlock stream_lock
        done
      ) work_queue in

      let handle_iq = function
        | _ -> Error (Raw.(xml_n "error" [ "type", "cancel" ] [
          xml Xmpp.stanzas "feature-not-implemented" [] []
        ]))
      in
      let handle_presence = function
        | _ -> Error "Presence not yet impl'd"
      in
      let handle_message = function
        | _ -> Error "Messages not yet impl'd"
      in
      let finished = ref false in
      let res = ref (Ok ()) in
      while !finished = false do
        (expect P.tree >>| X.from_raw >>= fun raw ->
          ((raw |> Xmpp.Stanza.Iq.of_xml) >>= fun { req_id; iq_type } ->
            let (ret_type, body) = match handle_iq iq_type with
              | Ok inner -> ("result", inner)
              | Error err -> ("error", err)
            in
            Ok (let respns =
              Raw.(xml_n "iq" [ "type", ret_type; "id", req_id ] [ body ])
              in Dispatch.dispatch user respns)
          ) <|>
          ((raw |> Xml.Check.(tag "presence" *> children)) >>= fun chs ->
            handle_presence chs
          ) <|>
          ((raw |> Xml.Check.(tag "message" *> children)) >>= fun chs ->
            handle_message chs
          ) ) |> function
            | Ok _ -> ()
            | err -> res := err; finished := true
      done;
      Dispatch.client_disconnected user; (* remove dispatch access as soon as possible *)
      Mutex.lock stream_lock; (* make sure worker is no longer writing to stream *)
        Thread.kill worker;
      Mutex.unlock stream_lock;
      respond "</stream:stream>";
      !res
    ) |> function
    | Ok _ -> print_endline "[SUCCESS]"; (* respond "</stream:stream>" *)
    | Error err -> respond "</stream:stream>"; failwith err;

  in
  Unix.(establish_server per_client (ADDR_INET (inet_addr_loopback,5222)))

let () = sv_start ()
