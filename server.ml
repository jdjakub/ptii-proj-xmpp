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

  module Stream = struct
    type error = Bad_format | Bad_ns_prefix | Internal | Invalid_from | Invalid_ns | Invalid_xml | Not_authorized | Not_well_formed | Reset
  end

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

    (* text | < tag attr_val* ( /> | > branch(tag) ) *)
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
    orig  : string;
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
      orig = P.Raw.to_string orig;
    }

  let from_raw xml = from_raw_br
    (function | "" -> Some "" | _ -> None)
    "en" xml
end

module Xmpp = struct
  let none = ("","")
  let streams = ("","urn:ietf:params:xml:ns:xmpp-streams")
  let stanzas = ("","urn:ietf:params:xml:ns:xmpp-stanzas")
  let sasl    = ("","urn:ietf:params:xml:ns:xmpp-sasl")
  let bind    = ("","urn:ietf:params:xml:ns:xmpp-bind")
  let jstream  = ("stream","http://etherx.jabber.org/streams")
  let jclient  = ("client","jabber:client")
  let jserver  = ("server","jabber:server")
  let jroster  = ("","jabber:iq:roster")
end

open Rresult

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
    let expect p = expect (String.make 2000 '.') (input from_ie) respond p in
    let stream_handshake id =
      expect A.(P.xml_decl *> P.tag_open) >>| X.from_raw >>= fun (X.Xml xml) ->
      if xml.tag <> ("stream","stream") then Error "Didn't get a <stream>" else
      match xml.attr "to" with | None -> Error "No 'to' address" | Some my_addr ->
      let response = Raw.(to_string_open (xml_d Xmpp.jstream "stream" [
        "xmlns", snd Xmpp.jclient;
        "version", "1.0"; "from", my_addr; "id", id;
      ] []))
      in respond response; Ok (my_addr, id)
    in
    ( stream_handshake "totally-random-id" >>= fun (my_addr,id) ->
      respond_tree Raw.(xml Xmpp.jstream "features" [] [
        xml Xmpp.sasl "mechanisms" [] [
          xml_n "mechanism" [] [ text "PLAIN" ];
          xml_n "required" [] []
        ]
      ]);
      expect P.tree >>| X.from_raw >>= fun (X.Xml xml) ->
      if snd xml.tag <> "auth" then Error "Didn't get a <auth>" else
      match xml.attr "mechanism" with None -> Error "No auth mechanism" | Some "PLAIN" ->
      match xml.child with [] -> Error "Empty credentials" | (X.Text (_,garbled)) :: _ ->

      let ungarbled = B64.decode garbled in
      let Ok (user,pass) = plain_auth_extract ungarbled in
      print_endline ("USERNAME: " ^ user);
      print_endline ("PASSWORD: " ^ pass);
      respond_tree Raw.(xml Xmpp.sasl "success" [] []);

      stream_handshake "totally-different-id" >>= fun (my_addr,id) ->
      respond_tree Raw.(xml Xmpp.jstream "features" [] [
        xml Xmpp.bind "bind" [] [ xml_n "required" [] [] ]
      ]);
      expect P.tree >>| X.from_raw >>= fun (X.Xml xml) ->
      if snd xml.tag <> "iq" then Error "Not iq" else
      match xml.attr "type" with None -> Error "No type attrib" | Some "set" ->
      match xml.attr "id" with None -> Error "No iq id" | Some rqid ->
      match xml.child with [ X.Xml xml ] ->
      if snd xml.tag <> "bind" then Error "Not bind" else
      match xml.child with [ X.Xml xml ] ->
      if snd xml.tag <> "resource" then Error "Not resource" else
      match xml.child with [ X.Text (_,resource) ] ->

      let jid = format "%s@%s/%s" user my_addr resource in
      respond_tree Raw.(xml_n "iq" [ "id", rqid; "type", "result" ] [
        xml Xmpp.bind "bind" [] [
          xml_n "jid" [] [ text jid ]
        ]
      ]);
      expect P.tree >>| X.from_raw >>= fun (X.Xml xml) ->
      (* Assuming it was the iq type=set { session } thing, which Psi does *)
      match xml.attr "id" with None -> Error "No iq id!" | Some rqid ->

      respond_tree Raw.(xml_n "iq" [ "type", "result"; "id", rqid ] []);
      expect P.tree >>| X.from_raw >>= fun (X.Xml xml) ->
      (* Assuming iq type=get { jroster:query } *)
      match xml.attr "id" with None -> Error "No iq id!!" | Some rqid ->

      respond_tree Raw.(xml_n "iq" [ "type", "result"; "id", rqid ] [
        xml Xmpp.jroster "query" [] [
          xml_n "item" [ "jid", "superphreak@smart.net"; "name", "Superphreak" ] [];
          xml_n "item" [ "jid", "scowlingmask@hackernet.det.usa"; "name", "UNKNOWN" ] [];
          xml_n "item" [ "jid", "disarray@scoria.tk"; "name", "DisArray" ] [];
        ]
        (*
        xml_n "error" [ "type", "cancel" ] [
          xml Xmpp.stanzas "service-unavailable" [] []
        ]
        *)
      ]);
      expect P.tree
    ) |> function
    | Ok _ -> print_endline "[SUCCESS]"; (* respond "</stream:stream>" *)
    | Error err -> respond "</stream:stream>"; failwith err
  in
  Unix.(establish_server per_client (ADDR_INET (inet_addr_loopback,5222)))

let () = sv_start ()
