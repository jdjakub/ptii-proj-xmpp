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
  val expect_ = Xml.buffered_expect in_c

  method expect p = expect_ p
  method respond str = output_string to_sv str; flush to_sv
  method respond_tree tree = self#respond Raw.(to_string tree)

  method establish_streams =
    self#respond ("<?xml version='1.0'?>" ^ Raw.(to_string_open (xml_d Xmpp.jstream "stream" [
      "xmlns", snd Xmpp.jclient;
      "version", "1.0"; "to", svname;
    ] [])) );
    self#expect Xml.P.tag_open >>| Xml.from_raw >>=
      Xml.Check.(qtag Xmpp.jstream "stream" *> attr "id") >>= fun id ->
        stream_id <- id; Ok ()

  method handshake =
    self#establish_streams;
    self#expect Xml.P.tree >>| Xml.from_raw >>=
      Xml.Check.(tag "features") >>= fun _ ->
    self#respond_tree Raw.(xml Xmpp.sasl "auth" [ "mechanism", "PLAIN" ] [
      text (B64.encode ("\x00" ^ user ^ "\x00" ^ password))
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
    ]);

    self#expect Xml.P.tree >>| Xml.from_raw >>= Xmpp.Stanza.Iq.of_xml >>=
    fun { req_id="iq-rost"; iq_type=(Result (Some xml)) } ->

    xml |> Xml.Check.(qtag Xmpp.jroster "query" *> children) >>= fun items ->

    (* <item jid name subscription /> *)

    self#respond_tree Raw.(xml_n "presence" [] []);

    Ok (List.map Xmpp.Roster.item_of_xml items)

  method message recpt body =
    let attrs = [ "type", "chat"; "to", recpt ^ "@" ^ svname ] in
    self#respond_tree
      Raw.(xml_n "message" attrs [
        body
      ])

  method message_t recpt body =
    self#message recpt Raw.(xml_n "body" [] [text body])

  method disconnect =
    self#respond_tree Raw.(xml_n "presence" [ "type", "unavailable" ] []);
    self#respond "</stream:stream>"

  method spill =
    let open Angstrom in
    self#expect Xml.P.(tree <|>
      (tag_close ("stream","stream") *>
        return Raw.(xml_n "stream" [] [ text "Closed stream" ])
      )
    )

end
