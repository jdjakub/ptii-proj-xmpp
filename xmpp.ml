let format = Printf.sprintf
let pair = fun x y -> (x,y)

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
      let process rstr_res tree = rstr_res >>= fun (i,rstr) -> (*print_endline (string_of_int i);*) item_of_xml tree
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
end
