let format = Printf.sprintf
let pair = fun x y -> (x,y)

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
      if t' = t then Ok (Xml xml) else Error t'
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
    if v = v' then Ok (Xml xml) else
      let t = snd xml.tag in
      let error = format "Expected <%s ... %s=\"%s\" ...>, got %s=\"%s\"" t k v k v'  in
      Error error
end

module Ag = Angstrom

let expect buf_r fill p =
  let open Ag.Buffered in
  let rec run = function
    | Partial next ->
      let { buffer; off; len } = !buf_r in
      if len <> 0 then
        let inp = Bigstring.sub buffer off len in
        (*print_endline ("Parsing: \n" ^ Bigstring.to_string inp ^ "\n");*)
        buf_r := { buffer; off; len=0 };
        run (next (`Bigstring inp))
      else
        let len_read = fill buffer in
        buf_r := { buffer; off=0; len=len_read };
        (*print_endline ("Read: " ^ string_of_int len_read);*)
        if len_read = 0 then Error "Didn't get anything"
        else
          let inp = Bigstring.sub buffer 0 len_read in
          let () = print_endline ("[IN]: " ^ Bigstring.to_string inp ^ "[/IN]") in
          run (next (`Bigstring inp))
    | Fail (unc,strs,str) ->
        Error (format "Parse error:\n%s\n%s\n" str (String.concat "\n" strs))
    | Done (buf,result) ->
        buf_r := buf;
        (*let rest = Bigstring.sub buf.buffer buf.off buf.len in*)
        (*print_endline (format "[%d+%d] unconsumed:\n%s\n" buf.off buf.len (Bigstring.to_string rest));*)
        Ok result
  in run (parse p)

let buffered_expect in_ch =
  let hackbuf = Bytes.create 512 in
  let fill_buf buf =
    (*print_endline "Filling";*)
    (*let foo = "<?xml version='1.0'?><stream:stream></stream:stream><foo></foo>" in*)
    (* Bytes.blit foo 0 hackbuf 0 (String.length foo); *)
    let num_read = input in_ch hackbuf 0 (Bytes.length hackbuf) (* String.length foo*) in
    Bigstring.blit_of_bytes hackbuf 0 buf 0 num_read;
    num_read
  in
  let buf = ref { Ag.Buffered.buffer = Bigstring.create (Bytes.length hackbuf); off = 0; len = 0 } in
  let expect p = expect buf fill_buf p in
  expect
