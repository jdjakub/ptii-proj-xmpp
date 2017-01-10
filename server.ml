let format = Printf.sprintf
let pair = fun x y -> (x,y)

module Xml = struct
  type attr = (string * string) * string
  type xml_open = string * string * attr list
  type xml_close = string * string
  type xml_node = Text of string | Xml of xml_open * xml_node list

  let string_of_qn = function
    | ("",name) -> name
    | (ns,name) -> ns ^ ":" ^ name

  let string_of_attrs attrs =
    let to_string (qn,value) =
        format " %s=\"%s\"" (string_of_qn qn) value (* DOES NOT SANITISE VAL!! *)
    in
    String.concat "" (List.map to_string attrs)

  let to_string_open (ns,tag,attrs) = format "<%s%s>" (string_of_qn (ns,tag)) (string_of_attrs attrs)

  let to_string_single (ns,tag,attrs) = format "<%s%s/>" (string_of_qn (ns,tag)) (string_of_attrs attrs)

  let to_string_close (ns,tag) = format "</%s>" (string_of_qn (ns,tag))

  let rec to_string = function
    | Text str -> str
    | Xml ((ns,tag,attrs),children) ->
      match children with
      | [] -> to_string_single (ns,tag,attrs)
      | _  -> let interior = String.concat "" (List.map to_string children) in
              to_string_open (ns,tag,attrs) ^ interior ^ to_string_close (ns,tag)
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
    lift2 (fun attr_k attr_v -> attr_k, attr_v)
      qual_name
      (tok_equ *> tok_string)

  (* <?xml version="1.0" ?> *)
  let xml_decl = lex (string "<?xml") *> many attr_val <* lex (string "?>")

  open Xml
   (* < tag (attr=val)* > *)
  let tag_open =
    tok_langle *> qual_name >>= fun (ns,id) ->
      lift2 (fun attrs _ -> (ns,id,attrs))
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
    (take_while1 (function | '<' -> false | _ -> true) >>| (fun t -> Text t)) <|>
    ( tok_langle *> qual_name >>= fun (ns,id) ->
            lift2 (fun attrs children -> Xml ((ns, id, attrs), children))
              (many attr_val)
              (tok_leaf *> return [] <|> tok_rangle *> branch t_rec (ns,id)) ) )

end

open Angstrom
module X = Xml
open Rresult

(* expect : string -> (string -> int -> int -> int) -> (string -> unit) -> 'a t -> ('a, string) result *)
let expect buf read respond p =
  let open Buffered in
  let rec run = function
    | Partial next ->
        let len = read buf 0 2000 in
        let inp = String.sub buf 0 len in
        print_endline ("[IN]: " ^ inp);
        run (next (`String inp))
    | Fail (unc,strs,str) ->
        Error (format "Parse error:\n%s\n%s\n" str (String.concat "\n" strs))
    | Done (unc,result) ->
        Ok result (* THROWS AWAY UNCONSUMED!!! *)
  in run (parse p)

let sv_start () =
  let per_client from_ie to_ie =
    let respond str = print_endline ("[OUT]: " ^ str); output_string to_ie str; flush to_ie in
    let expect p = expect (String.make 2000 '.') (input from_ie) respond p
    in
    (expect P.(xml_decl *> tag_open) >>= fun ((_,"stream",attrs) as xml) ->
      let my_addr = List.assoc ("","to") attrs in
      let response = "<?xml version=\"1.0\"?>" ^
            X.to_string_open ("stream","stream",[
              (("xmlns","stream"),"http://etherx.jabber.org/streams");
              (("","xmlns"),"jabber:client");
              (("","version"),"1.0");
              (("","from"),my_addr);
              (("","id"),"totally-random-id");
              (("xml","lang"),"en"); ]) ^
            X.to_string (X.Xml (("stream","features",[]),[]))
      in
      respond response;
      expect P.tree >>= fun xml ->
      xml |> function
        | Ok _ -> print_endline "[SUCCESS]"
        | Error err -> failwith err
  in
  Unix.(establish_server per_client (ADDR_INET (inet_addr_loopback,5222)))

let () = sv_start ()
