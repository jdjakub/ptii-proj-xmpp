open Unix

let () =
let sock = socket PF_INET SOCK_STREAM 0 in
let addr = inet_addr_of_string "127.0.0.1" in
  connect sock (ADDR_INET (addr, 12345));
  let str = String.make 20 '.' in
  let len = read sock str 0 20 in
  let str = String.sub str 0 len in
    Printf.printf "SV: %s\n" str;
