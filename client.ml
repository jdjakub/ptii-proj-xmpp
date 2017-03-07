open Unix

open Rresult

module Raw = Xml.P.Raw

let password = "pass"
let resource = "res"

class client user svname =
let (in_c,out_c) = open_connection (ADDR_INET (inet_addr_loopback,5222)) in
object (self)
  val from_sv = in_c
  val to_sv = out_c
  val mutable stream_id = "0"

  method expect p = Error "unimplemented"
  method respond str = output_string to_sv str; flush to_sv
  method respond_tree tree = self#respond Raw.(to_string tree)

  method establish_streams =
    self#respond "<?xml version='1.0'?>";
    self#respond Raw.(to_string_open (xml_d Xmpp.jstream "stream" [
      "xmlns", snd Xmpp.jclient;
      "version", "1.0"; "to", svname;
    ] []));
    self#expect Xml.P.tag_open >>| Xml.from_raw >>=
      Xml.Check.(qtag Xmpp.jstream "stream" *> attr "id") >>= fun id ->
        stream_id <- id; Ok ()

  method handshake =
    self#establish_streams;
    self#expect Xml.P.tree >>| Xml.from_raw >>=
      Xml.Check.(tag "features") >>= fun _ ->
    self#respond_tree Raw.(xml Xmpp.sasl "auth" [ "mechanism", "PLAIN" ] [
      text ("\x00" ^ user ^ "\x00" ^ password)
    ]);
    self#expect Xml.P.tree >>| Xml.from_raw >>=
      Xml.Check.(tag "success") >>= fun _ ->

    self#establish_streams;
    self#expect Xml.P.tree >>| Xml.from_raw >>=
      Xml.Check.(tag "features") >>= fun _ ->
    self#respond_tree Raw.(xml_n "iq" [ "type", "set"; "id", "iq-bind" ] [
      xml Xmpp.bind "bind" [] [
        xml_n "resource" [] [
          text resource
        ]
      ]
    ]);
    self#expect Xml.P.tree >>| Xml.from_raw >>= Xmpp.Stanza.Iq.of_xml >>=
    fun { req_id="iq-bind"; iq_type=(Result (Some xml)) } ->

    self#respond_tree Raw.(xml_n "iq" [ "type", "set"; "id", "iq-sess" ] [
      xml Xmpp.session "session" [] []
    ]);

    self#expect Xml.P.tree >>| Xml.from_raw >>= Xmpp.Stanza.Iq.of_xml >>=
    fun { req_id="iq-sess"; iq_type=(Result None) } ->

    self#respond_tree Raw.(xml_n "iq" [ "type", "get"; "id", "iq-rost" ] [
      xml Xmpp.jroster "query" [] []
    ]); Ok ()

end

let () =
let sock = socket PF_INET SOCK_STREAM 0 in
let addr = inet_addr_of_string "127.0.0.1" in
  connect sock (ADDR_INET (addr, 12345));
  let str = String.make 20 '.' in
  let len = read sock str 0 20 in
  let str = String.sub str 0 len in
    Printf.printf "SV: %s\n" str;
