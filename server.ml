open Unix
let format = Printf.sprintf
let pair = fun x y -> (x,y)

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
  let prologue = lex (string "<?xml") *> many attr_val <* lex (string "?>")

  type attr = (string * string) * string
  type xml_node = Xml of string * string * attr list * xml_node list


   (* < tag (attr=val)* > *)
  let tag_open =
    tok_langle *> qual_name >>= fun (ns,id) ->
      lift2 (fun attrs _ -> Xml (ns,id,attrs,[]))
        (many attr_val)
        tok_rangle

  (* </ tag > *)
  let tag_close (ns,id) =
    tok_close *> qual_name >>= fun (ns',id') ->
      if ns = ns' && id = id'
        then tok_rangle else fail (format "Expected tag %s:%s, got %s:%s" ns id ns' id')

  (* branch = </ tag > | tree branch *)
  let branch t_rec tagname = fix ( fun b_rec ->
    tag_close tagname *> return [] <|>
      (t_rec >>= fun tree -> b_rec >>| fun trees -> tree::trees) )

  (* < tag attr_val* ( /> | > branch(tag) ) *)
  let tree = fix ( fun t_rec ->
    tok_langle *> qual_name >>= fun (ns,id) ->
      lift2 (fun attrs children -> Xml (ns, id, attrs, children))
        (many attr_val)
        (tok_leaf *> return [] <|> tok_rangle *> branch t_rec (ns,id)) )

end

open Angstrom

let await () =
let sock_incoming = socket PF_INET SOCK_STREAM 0 in
  bind sock_incoming (ADDR_INET (inet_addr_loopback,12345));
  listen sock_incoming 1;
  let (sock_cl, cl_addr) = accept sock_incoming in
  let (addr, port) = match cl_addr with
    ADDR_UNIX str   -> (str,0)
  | ADDR_INET (a,p) -> (string_of_inet_addr a,p)
  in
  Printf.printf "Connection from %s:%d\n" addr port;
  let buf = String.make 2000 '0' in
  let len = read sock_cl str 0 2000 in
  let inp = `String (String.sub str 0 len) in
  match parse_only (lift2 P.prologue P.tag_open pair) inp) with
    | Result.Error str -> failwith str
    | Result.Ok (attrs,Xml(ns,id,attrs,[]) -> failwith "undefined"
